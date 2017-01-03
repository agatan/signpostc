def times(n: Int, f: Unit => Unit) {
    if n > 0 {
        f();
        times(n - 1, f);
    }
}

def main() {
    times(3, || { print_string("Hello"); });
    times(3, || print_string("Hello"));
    3.times(|| {
        print_string("Hello");
    });
}
