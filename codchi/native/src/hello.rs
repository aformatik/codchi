// use msgbox::IconType;
// use std::env;
use floem::reactive::*;
use floem::view::*;
use floem::views::*;
use floem::*;

// fn main() {
//     let args: Vec<String> = env::args().collect();

//     let name = &args[1];

//     let msg = format!("Hello {} from rust", name);

//     msgbox::create("Hello Title", msg.as_str(), IconType::Info);
// }
//
fn app_view() -> impl View {
    // create a counter reactive signal with initial value 0
    let (counter, set_counter) = create_signal(0);

    // create user interface with Floem view functions
    stack((
        label(move || format!("Value: {}", counter.get())),
        stack((
            text("Increment").on_click(move |_| {
                set_counter.update(|value| *value += 1);
                EventPropagation::Stop
            }),
            text("Decrement").on_click(move |_| {
                set_counter.update(|value| *value -= 1);
                EventPropagation::Stop
            }),
        )),
    ))
}

fn main() {
    floem::launch(app_view);
}
