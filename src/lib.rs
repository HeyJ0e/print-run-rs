use proc_macro::TokenStream;
use quote::quote;
use syn::{ItemFn, parse_macro_input};

#[proc_macro_attribute]
pub fn print_run(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemFn);
    let syn::ItemFn {
        attrs,
        vis,
        sig,
        block,
    } = input;

    let fn_name = &sig.ident;

    // Wrap the original function body
    let new_block = quote! {
        {
            println!("{} starting", stringify!(#fn_name));
            let result = (|| #block)();
            println!("{} ended", stringify!(#fn_name));
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
