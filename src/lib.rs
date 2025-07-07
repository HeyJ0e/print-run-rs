use nu_ansi_term::{Color, ansi::RESET};
use proc_macro::TokenStream;
use quote::quote;
use std::sync::Once;
use syn::{
    Attribute, Error, Ident, Item, ItemFn, ItemMod, Result, Token,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
};

static IS_DEPTH_MODULE_ADDED: Once = Once::new();

/// Optional input flags
#[derive(Debug, Default)]
struct PrintRunArgs {
    colored: bool,
    duration: bool,
    indent: bool,
    timestamps: bool,
}

impl Parse for PrintRunArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut args = PrintRunArgs::default();

        while !input.is_empty() {
            let ident: Ident = input.parse()?;
            let _ = input.parse::<Option<Token![,]>>(); // allow optional commas

            match ident.to_string().as_str() {
                "colored" => args.colored = true,
                "duration" => args.duration = true,
                "indent" => args.indent = true,
                "timestamps" => args.timestamps = true,
                other => {
                    return Err(Error::new(
                        ident.span(),
                        format!("Unknown attribute '{}'", other),
                    ));
                }
            }
        }

        Ok(args)
    }
}

macro_rules! or_else {
    ($cond:expr, $true_:expr, $false_:expr) => {
        if $cond { $true_ } else { $false_ }
    };
}

macro_rules! or_nothing {
    ($cond:expr, $true_:expr) => {
        or_else!($cond, $true_, quote! {})
    };
}

macro_rules! or_empty_str {
    ($cond:expr, $true_:expr) => {
        or_else!($cond, $true_, quote! { || "".to_string() })
    };
}

macro_rules! colorize {
    ($txt:expr, $col_name: ident) => {
        format!("{}{}{}", Color::$col_name.prefix().to_string(), $txt, RESET)
    };
}

macro_rules! colorize_fn {
    ($color_name: ident) => {{
        let color = Color::$color_name.prefix().to_string();
        quote! {
            |txt: String| format!("{}{}{}", #color, txt, #RESET)
        }
    }};
}

macro_rules! create_timestamp {
    ($colored:expr) => {{
        let colorize = or_else!(
            $colored,
            colorize_fn!(DarkGray),
            quote! { |txt: String| txt }
        );
        quote! {
            || {
                let now = std::time::SystemTime::now();
                let epoch = now
                    .duration_since(std::time::UNIX_EPOCH)
                    .expect("Time went backwards");

                let total_secs = epoch.as_secs();
                let millis = epoch.subsec_millis();

                let hours = (total_secs / 3600) % 24;
                let minutes = (total_secs / 60) % 60;
                let seconds = total_secs % 60;
                let ts = format!("{:02}:{:02}:{:02}.{:03}", hours, minutes, seconds, millis);
                let ts = {#colorize}(ts);
                format!("{} ", ts) // don't colorize the separator
            }
        }
    }};
}

macro_rules! create_duration {
    ($colored:expr) => {{
        let colorize = or_else!($colored, colorize_fn!(Green), quote! { |txt: String| txt });
        quote! {
            |start: std::time::Instant| {
                let elapsed = start.elapsed().as_nanos();
                let dur =
                    if elapsed < 1_000 {
                        format!("{}ns", elapsed)
                    } else if elapsed < 1_000_000 {
                        format!("{:.2}µs", elapsed as f64 / 1_000.0)
                    } else if elapsed < 1_000_000_000 {
                        format!("{:.2}ms", elapsed as f64 / 1_000_000.0)
                    } else {
                        format!("{:.2}s", elapsed as f64 / 1_000_000_000.0)
                    }
                ;
                let dur = {#colorize}(dur);
                format!(" in {}", dur) // colorize the result only
            }
        }
    }};
}

macro_rules! create_indent {
    ($val:expr, $ch:expr) => {{
        let val = $val;
        let ch = $ch;
        quote! {
            || crate::__print_run_depth::DEPTH.with(|depth| {
                let depth_val = *depth.borrow();
                *depth.borrow_mut() = depth_val.saturating_add_signed(#val);
                let depth_val= depth_val.saturating_add_signed((#val-1) / 2);
                let spaces = "┆ ".repeat(depth_val);
                format!("{}{} ", spaces, #ch)
            })
        }
    }};
}

#[proc_macro_attribute]
pub fn print_run(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as PrintRunArgs);
    let PrintRunArgs {
        colored,
        duration,
        indent,
        timestamps,
    } = args;
    let input = parse_macro_input!(item as ItemFn);
    let ItemFn {
        attrs,
        vis,
        sig,
        block,
    } = input;
    let fn_name = sig.ident.to_string();

    // Create start/end function names
    let start = or_else!(colored, colorize!(fn_name.clone(), Yellow), fn_name.clone());
    let end = or_else!(colored, colorize!(fn_name.clone(), Blue), fn_name.clone());

    // Create timestamp creator closure
    let create_timestamp_fn = or_empty_str!(timestamps, create_timestamp!(colored));

    // Create duration creator closure
    let duration_fn = or_else!(
        duration,
        create_duration!(colored),
        quote! { |_| "".to_string() }
    );

    // Create indent creator closures
    let indent_top = or_empty_str!(indent, create_indent!(1isize, "┌"));
    let indent_bottom = or_empty_str!(indent, create_indent!(-1isize, "└"));

    // Wrap the original function body
    let new_block = quote! {
        {
            let ts = {#create_timestamp_fn}();
            let start = std::time::Instant::now();
            let indent = {#indent_top}();
            println!("{}{}{} starting", ts, indent, #start);

            let result = (|| #block)();

            let dur = {#duration_fn}(start);
            let ts = {#create_timestamp_fn}();
            let indent = {#indent_bottom}();
            println!("{}{}{} ended{}", ts, indent, #end, dur);
            result
        }
    };

    // Define support module with DEPTH if it runs for the first time
    let mut define = false;
    IS_DEPTH_MODULE_ADDED.call_once(|| define = true);
    let module = or_nothing!(
        define,
        quote! {
            #[doc(hidden)]
            pub(crate) mod __print_run_depth {
                use std::cell::RefCell;
                thread_local! {
                    pub static DEPTH: RefCell<usize> = RefCell::new(0);
                }
            }
        }
    );

    // Reconstruct the function
    let output = quote! {
        #(#attrs)*
        #vis #sig #new_block
        #module
    };

    TokenStream::from(output)
}

#[derive(Debug, Default)]
struct AutoPrintRunArgs {
    args: Vec<Ident>,
}

impl Parse for AutoPrintRunArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut args = Vec::new();
        while !input.is_empty() {
            args.push(input.parse()?);
            let _ = input.parse::<Option<Token![,]>>()?; // allow optional commas
        }
        Ok(Self { args })
    }
}

#[proc_macro_attribute]
pub fn auto_print_run(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as AutoPrintRunArgs);
    let mut input = parse_macro_input!(item as ItemMod);

    let arg_idents = &args.args;
    let fn_macro: Attribute = parse_quote! {
        #[print_run( #(#arg_idents),* )]
    };

    // Add the macro attribute to all functions in the module
    if let Some((_, ref mut items)) = input.content {
        for item in items {
            if let Item::Fn(func) = item {
                func.attrs.push(fn_macro.clone());
            }
        }
    } else {
        return Error::new_spanned(&input, "`#[auto_print_run]` only supports inline modules")
            .to_compile_error()
            .into();
    }

    TokenStream::from(quote! { #input })
}
