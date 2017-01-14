enum Option<T> {
    None,
    Some(T),
}

enum Result<T, E> {
    Ok(T),
    Err(E)
}

struct Person {
    name: String,
    age: Int,
}

