use print_run::print_run;
use tokio;

#[tokio::test]
async fn async_test() {
    #[print_run]
    mod sub {
        pub async fn sub_1() {
            msg!("inside sub 1");
            sub_2().await;
        }

        pub async fn sub_2() {
            msg!("inside sub 2");
            sub_3().await;
        }

        pub async fn sub_3() {
            msg!("inside sub 3");
        }
    }

    sub::sub_1().await;
}
