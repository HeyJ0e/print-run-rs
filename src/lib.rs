use nu_ansi_term::{Color, ansi::RESET};
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Error, Ident, ItemFn, Result, Token,
    parse::{Parse, ParseStream},
    parse_macro_input,
};

/// Optional input flags
#[derive(Debug, Default)]
struct PrintRunArgs {
    colored: bool,
    duration: bool,
    timestamps: bool,
}

impl Parse for PrintRunArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut args = PrintRunArgs::default();

        while !input.is_empty() {
            let ident: Ident = input.parse()?;
            let _ = input.parse::<Option<Token![,]>>(); // allow commas or not

            match ident.to_string().as_str() {
                "colored" => args.colored = true,
                "duration" => args.duration = true,
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

macro_rules! cond {
    ($cond:expr, $true_:expr, $false_:expr) => {
        if $cond { $true_ } else { $false_ }
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
        let colorize = cond!(
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
        let colorize = cond!($colored, colorize_fn!(Yellow), quote! { |txt: String| txt });
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

#[proc_macro_attribute]
pub fn print_run(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as PrintRunArgs);
    let input = parse_macro_input!(item as ItemFn);
    let ItemFn {
        attrs,
        vis,
        sig,
        block,
    } = input;
    let PrintRunArgs {
        colored,
        duration,
        timestamps,
    } = args;
    let fn_name = sig.ident.to_string();

    // Create start/end function names
    let start = cond!(colored, colorize!(fn_name.clone(), Green), fn_name.clone());
    let end = cond!(colored, colorize!(fn_name.clone(), Cyan), fn_name.clone());

    // Create timestamp creator closure
    let create_timestamp_fn = cond!(
        timestamps,
        create_timestamp!(colored),
        quote! { || "".to_string() }
    );

    // Create duration creator closures
    let duration_fn = cond!(
        duration,
        create_duration!(colored),
        quote! { |_| "".to_string() }
    );

    // Wrap the original function body
    let new_block = quote! {
        {
            let ts = {#create_timestamp_fn}();
            let start = std::time::Instant::now();
            println!("{}{} starting", ts, #start);

            let result = (|| #block)();

            let dur = {#duration_fn}(start);
            let ts = {#create_timestamp_fn}();
            println!("{}{} ended{}", ts, #end, dur);
            result
        }
    };

    // Reconstruct the function
    let output = quote! {
        #(#attrs)*
        #vis #sig #new_block
    };

    TokenStream::from(output)
}
