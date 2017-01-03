def inc(x: Ref<Int>) {
    x := @x + 1;
}

def main() {
    let x = ref(0);
    inc(x);
    x.inc();
    x.inc;
    print_int(@x);
}
