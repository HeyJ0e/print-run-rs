use nu_ansi_term::{Color, ansi::RESET};
use proc_macro::TokenStream;
use quote::{ToTokens, format_ident, quote};
use std::sync::{Once, OnceLock};
use syn::{
    Attribute, Error, FnArg, Ident, ImplItem, ImplItemFn, Item, ItemFn, ItemMod, LitStr, Result,
    Token,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
};

static IS_HELPER_MODULE_ADDED: Once = Once::new();
static PRINT_RUN_DEFAULTS: OnceLock<PrintRunArgs> = OnceLock::new();

/// Optional input flags
#[derive(Debug, Default)]
struct PrintRunArgs {
    colored: Option<bool>,
    duration: Option<bool>,
    indent: Option<bool>,
    supress_labels: Option<bool>,
    timestamps: Option<bool>,
    __struct_prefix: Option<String>,
}

impl PrintRunArgs {
    pub fn to_idents(&self) -> Vec<Ident> {
        let mut result = Vec::new();

        if self.colored == Some(true) {
            result.push(format_ident!("colored"));
        }
        if self.duration == Some(true) {
            result.push(format_ident!("duration"));
        }
        if self.indent == Some(true) {
            result.push(format_ident!("indent"));
        }
        if self.supress_labels == Some(true) {
            result.push(format_ident!("supress_labels"));
        }
        if self.timestamps == Some(true) {
            result.push(format_ident!("timestamps"));
        }

        result
    }

    pub fn merge_with(&self, override_args: &PrintRunArgs) -> PrintRunArgs {
        PrintRunArgs {
            colored: override_args.colored.or(self.colored),
            duration: override_args.duration.or(self.duration),
            indent: override_args.indent.or(self.indent),
            timestamps: override_args.timestamps.or(self.timestamps),
            supress_labels: override_args.supress_labels.or(self.supress_labels),
            __struct_prefix: override_args
                .__struct_prefix
                .clone()
                .or_else(|| self.__struct_prefix.clone()),
        }
    }
}

impl Parse for PrintRunArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut args = PrintRunArgs::default();

        while !input.is_empty() {
            let ident: Ident = input.parse()?;
            let _ = input.parse::<Option<Token![,]>>(); // allow optional commas

            match ident.to_string().as_str() {
                "colored" => args.colored = Some(true),
                "duration" => args.duration = Some(true),
                "indent" => args.indent = Some(true),
                "supress_labels" => args.supress_labels = Some(true),
                "timestamps" => args.timestamps = Some(true),
                "__struct_prefix" => {
                    let _ = input.parse::<Option<Token![=]>>()?;
                    let lit: LitStr = input.parse()?;
                    args.__struct_prefix = Some(lit.value().to_string());
                }
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
    ($color_name: ident, "bold") => {{
        let color = Color::$color_name.bold().prefix().to_string();
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
    ($colored:expr, $supress_labels:expr) => {{
        let colorize = or_else!($colored, colorize_fn!(Green), quote! { |txt: String| txt });
        let supress_labels = $supress_labels;
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
                let dur = {#colorize}(dur); // colorize the result only
                if #supress_labels {
                    format!("[{}]", dur)
                } else {
                    format!(" in {}", dur)
                }
            }
        }
    }};
}

macro_rules! create_indent {
    ($val:expr, $ch:expr) => {{
        let val = $val;
        let ch = $ch;
        quote! {
            || crate::__print_run_helper::DEPTH.with(|depth| {
                let depth_val = *depth.borrow();
                *depth.borrow_mut() = depth_val.saturating_add_signed(#val);
                let depth_val= depth_val.saturating_add_signed((#val-1) / 2);
                let spaces = "┆ ".repeat(depth_val);
                format!("{}{} ", spaces, #ch)
            })
        }
    }};
}

fn get_print_run_defaults() -> Option<&'static PrintRunArgs> {
    PRINT_RUN_DEFAULTS.get()
}

#[proc_macro_attribute]
pub fn print_run_defaults(attr: TokenStream, input: TokenStream) -> TokenStream {
    let attr_clone = attr.clone();
    let args = parse_macro_input!(attr as PrintRunArgs);

    if PRINT_RUN_DEFAULTS.set(args).is_err() {
        return syn::Error::new_spanned(
            proc_macro2::TokenStream::from(attr_clone),
            "print run defaults already set",
        )
        .to_compile_error()
        .into();
    }

    input
}

#[proc_macro_attribute]
pub fn print_run(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut args = parse_macro_input!(attr as PrintRunArgs);
    if let Some(defaults) = get_print_run_defaults() {
        args = defaults.merge_with(&args);
    }

    // Try parsing as a function first
    if let Ok(func) = syn::parse::<ItemFn>(item.clone()) {
        return print_run_fn(args, func);
    }

    // Try parsing as a module
    if let Ok(module) = syn::parse::<ItemMod>(item.clone()) {
        return print_run_mod(args, module);
    }

    // Unsupported item — return error
    syn::Error::new_spanned(
        proc_macro2::TokenStream::from(item),
        "#[print_run] can only be used on functions or inline modules",
    )
    .to_compile_error()
    .into()
}

fn print_run_fn(args: PrintRunArgs, fn_item: ItemFn) -> TokenStream {
    let PrintRunArgs {
        colored,
        duration,
        indent,
        supress_labels,
        timestamps,
        __struct_prefix,
    } = args;
    let ItemFn {
        attrs,
        vis,
        sig,
        block,
    } = fn_item;
    let colored = colored == Some(true);
    let duration = duration == Some(true);
    let indent = indent == Some(true);
    let supress_labels = supress_labels == Some(true);
    let timestamps = timestamps == Some(true);

    // Create name with prefix
    let fn_name = sig.ident.to_string();
    let prefix = __struct_prefix.unwrap_or("".into());
    let fn_name = format!("{prefix}{fn_name}");

    // Create labels
    let start_label = or_else!(!supress_labels, "starting", "");
    let end_label = or_else!(!supress_labels, "ended", "");

    // Create start/end function names
    let start = or_else!(colored, colorize!(fn_name.clone(), Yellow), fn_name.clone());
    let end = or_else!(colored, colorize!(fn_name.clone(), Blue), fn_name.clone());

    // Create timestamp creator closure
    let create_timestamp_fn = or_empty_str!(timestamps, create_timestamp!(colored));

    // Create duration creator closure
    let duration_fn = or_else!(
        duration,
        create_duration!(colored, supress_labels),
        quote! { |_| "".to_string() }
    );

    // Create indent creator closures
    let indent_top = or_empty_str!(indent, create_indent!(1isize, "┌"));
    let indent_bottom = or_empty_str!(indent, create_indent!(-1isize, "└"));
    let indent_body = or_empty_str!(indent, create_indent!(0isize, ""));

    // Create msg! macro
    let colorize_msg = if colored {
        colorize_fn!(White, "bold")
    } else {
        colorize_fn!(Default, "bold")
    };
    let msg_macro = quote! {
        #[allow(unused)]
        macro_rules! msg {
            ($($arg:tt)*) => {{
                let ts = {#create_timestamp_fn}();
                let indent = {#indent_body}();
                let msg = format!($($arg)*);
                let msg = {#colorize_msg}(msg);
                println!("{}{} {}", ts, indent, msg);
            }};
        }
    };

    // Wrap the original function body
    let new_block = quote! {
        {
            let ts = {#create_timestamp_fn}();
            let start = std::time::Instant::now();
            let indent = {#indent_top}();
            println!("{}{}{} {}", ts, indent, #start, #start_label);
            #msg_macro

            let result = (|| #block)();

            let dur = {#duration_fn}(start);
            let ts = {#create_timestamp_fn}();
            let indent = {#indent_bottom}();
            println!("{}{}{} {}{}", ts, indent, #end, #end_label, dur);
            result
        }
    };

    // Add helper module if needed
    let helper_module = define_helper_module();

    // Reconstruct the function
    let output = quote! {
        #(#attrs)*
        #vis #sig #new_block
        #helper_module
    };

    TokenStream::from(output)
}

fn print_run_mod(args: PrintRunArgs, mut module_item: ItemMod) -> TokenStream {
    let arg_idents = args.to_idents();
    let fn_macro: Attribute = parse_quote! {
        #[print_run::print_run( #(#arg_idents),* )]
    };

    // Add the macro attribute to all functions and struct methods in the module
    if let Some((_, ref mut items)) = module_item.content {
        for item in items {
            match item {
                Item::Fn(func) => {
                    func.attrs.push(fn_macro.clone());
                }
                Item::Impl(item_impl) => {
                    let ty_str = (&item_impl.self_ty).into_token_stream().to_string();
                    for impl_item in &mut item_impl.items {
                        if let ImplItem::Fn(method) = impl_item {
                            let is_static = is_static_method(&method);
                            let ty_str = ty_str.clone() + if is_static { "::" } else { "." };
                            let fn_macro = parse_quote! {
                                #[print_run::print_run( #(#arg_idents),*, __struct_prefix=#ty_str )]
                            };
                            method.attrs.push(fn_macro);
                        }
                    }
                }
                _ => {}
            }
        }
    } else {
        return Error::new_spanned(
            module_item.mod_token,
            "`#[print_run]` only supports inline modules",
        )
        .to_compile_error()
        .into();
    }

    // Add helper module if needed
    let helper_module = define_helper_module();
    let module_tokens = module_item.into_token_stream();

    TokenStream::from(quote! { #module_tokens #helper_module })
}

fn define_helper_module() -> proc_macro2::TokenStream {
    // Define support module with DEPTH if it runs for the first time
    let mut define = false;
    IS_HELPER_MODULE_ADDED.call_once(|| define = true);
    or_nothing!(
        define,
        quote! {
            #[doc(hidden)]
            #[allow(unused)]
            pub(crate) mod __print_run_helper {
                use std::cell::RefCell;
                thread_local! {
                    pub static DEPTH: RefCell<usize> = RefCell::new(0);
                }
            }
        }
    )
}

fn is_static_method(method: &ImplItemFn) -> bool {
    match method.sig.inputs.first() {
        Some(FnArg::Receiver(_)) => false, // instance method
        Some(FnArg::Typed(_)) => true,     // static method (no self)
        None => true,                      // no args = static
    }
}
