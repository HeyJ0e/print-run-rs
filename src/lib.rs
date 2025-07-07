use colorize::colorize;
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
}

impl Parse for PrintRunArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut args = PrintRunArgs::default();

        while !input.is_empty() {
            let ident: Ident = input.parse()?;
            let _ = input.parse::<Option<Token![,]>>(); // allow commas or not

            match ident.to_string().as_str() {
                "colored" => args.colored = true,
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
    let fn_name = sig.ident.to_string();
    let PrintRunArgs { colored } = args;

    // Create start/end messages
    let start_fn = if colored {
        colorize!("{}", Fg->fn_name)
    } else {
        fn_name.clone()
    };
    let start_msg = format!("{} starting", start_fn);

    let end_fn = if colored {
        colorize!("{}", Fg->fn_name)
    } else {
        fn_name.clone()
    };
    let end_msg = format!("{} ended", end_fn);

    // Wrap the original function body
    let new_block = quote! {
        {
            println!("{}", #start_msg);
            let result = (|| #block)();
            println!("{}", #end_msg);
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
