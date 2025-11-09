mod codegen;
mod debug;
mod lexer;
mod parser;
mod typer;

fn compile(source: &str) -> String {
    let tokens = lexer::tokenize(source);
    let (ast, struct_decls, fn_decls, extern_fns) = parser::parse(&tokens);
    let (typed_ast, fn_instances) = typer::infer_types(ast, &struct_decls, &fn_decls, &extern_fns);
    // debug::print_ast(&typed_ast, 0, &fn_instances);
    let code = codegen::emit(
        typed_ast,
        fn_instances,
        &fn_decls,
        &struct_decls,
        &extern_fns,
    );
    code
}

fn main() {
    println!(
        "{}",
        compile(
            "struct Point { x: float, y: int } struct Shape { points: Point[3], radius: float } let main { let s = Shape { points: [ Point { x: 1.0, y: 3 }, Point { x: 5.0, y: 7 }, Point { x: 2.0, y: 3 } ], radius: 5.0 } s.points[1].y }"
        )
    );
}

// TODO:
// - Implement modulo operator
// - Implement numeric literal polymorphism, e.g. x + 1 works if 'x' is a float or int
// - Investigate tail recursion

#[cfg(test)]
mod tests {
    use super::compile;
    use std::fs::File;
    use std::io::Write;
    use std::process::Command;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static COUNTER: AtomicUsize = AtomicUsize::new(0);

    fn compile_and_run(src: &str) -> std::io::Result<i32> {
        let ir = compile(src);
        let id = COUNTER.fetch_add(1, Ordering::Relaxed);
        let mut ll_path = std::env::temp_dir();
        ll_path.push(format!("periphery_test_{}.ll", id));
        let exe_path = ll_path.with_extension("exe");
        File::create(&ll_path)?.write_all(ir.as_bytes())?;
        assert!(
            Command::new("clang")
                .arg("-Wno-override-module")
                .arg(&ll_path)
                .arg("-o")
                .arg(&exe_path)
                .status()?
                .success()
        );
        let code = Command::new(&exe_path).status()?.code().unwrap_or(-1);
        Ok(code)
    }

    fn expect_exit(src: &str, expected: i32) {
        assert_eq!(compile_and_run(src).unwrap(), expected);
    }

    #[test]
    fn extern_abs() {
        expect_exit("extern abs(int) -> int let main { abs(42) }", 42);
    }

    #[test]
    fn hello_world_puts() {
        expect_exit(
            "extern puts(*byte) -> int let main { puts(\"Hello world\") 0 }",
            0,
        );
    }

    #[test]
    fn constant_main() {
        expect_exit("let main { 7 }", 7);
    }

    #[test]
    fn fib_10() {
        expect_exit(
            "let fib n = if n < 2 then n else fib(n - 1) + fib(n - 2) let main { fib(10) }",
            55,
        );
    }

    #[test]
    fn var_decl_and_use() {
        expect_exit("let main { let a = 3 let b = 4 a + b }", 7);
    }

    #[test]
    fn fn_zero_params() {
        expect_exit("let f { 42 } let main { f() }", 42);
    }

    #[test]
    fn fn_two_params() {
        expect_exit("let add x y = x + y let main { add(10, 32) }", 42);
    }

    #[test]
    fn nested_calls() {
        expect_exit(
            "let inc x = x + 1 let add x y = x + y let main { inc(add(3,4)) }",
            8,
        );
    }

    #[test]
    fn unary_minus() {
        expect_exit("let main { -5 }", -5);
    }

    #[test]
    fn unary_not() {
        expect_exit("let main { if !false then 1 else 0 }", 1);
    }

    #[test]
    fn precedence_arith() {
        expect_exit("let main { 2 + 3 * 4 }", 14);
    }

    #[test]
    fn precedence_parens() {
        expect_exit("let main { (2 + 3) * 4 }", 20);
    }

    #[test]
    fn logic_and_short_circuit() {
        expect_exit("let main { if true and false then 1 else 2 }", 2);
    }

    #[test]
    fn logic_or_short_circuit() {
        expect_exit("let main { if false or 5 == 5 then 9 else 4 }", 9);
    }

    #[test]
    fn equality_and_relation() {
        expect_exit("let main { if 10 > 5 and 3 != 4 then 1 else 0 }", 1);
    }

    #[test]
    fn if_expression_true() {
        expect_exit("let main { if 1 < 2 then 10 else 20 }", 10);
    }

    #[test]
    fn if_expression_false() {
        expect_exit("let main { if 2 < 2 then 10 else 20 }", 20);
    }

    #[test]
    fn block_last_expr() {
        expect_exit("let main { { 1 2 3 4 5 } }", 5);
    }

    #[test]
    fn shadowing_var() {
        expect_exit("let x = 5 let main { { let x = 7 x } }", 7);
    }

    #[test]
    fn recursion_simple() {
        expect_exit(
            "let fact n = if n == 0 then 1 else n * fact(n - 1) let main { fact(6) }",
            720,
        );
    }

    #[test]
    fn struct_decl_only() {
        expect_exit("struct S { a: int, b: int } let main { 0 }", 0);
    }

    #[test]
    fn chained_calls() {
        expect_exit(
            "let a x = x + 1 let b x = x * 2 let c x = x - 3 let main { c(b(a(10))) }",
            ((10 + 1) * 2) - 3,
        );
    }

    #[test]
    fn boolean_ops_mix() {
        expect_exit(
            "let main { if (true and !false) or false then 11 else 22 }",
            11,
        );
    }

    #[test]
    fn complex_expression() {
        expect_exit("let main { if (1 + 2) * 3 == 9 then 10 else 11 }", 10);
    }

    #[test]
    fn poly_simple_int_float() {
        expect_exit(
            "let id x = x let main { if id(3) == 3 and id(4.0) == 4.0 then 1 else 0 }",
            1,
        );
    }

    #[test]
    fn poly_add_int_and_float() {
        expect_exit(
            "let add x y = x + y let main { if add(2,3) == 5 and add(2.5,3.5) == 6.0 then 7 else 0 }",
            7,
        );
    }

    #[test]
    fn poly_nested_calls() {
        expect_exit(
            "let id x = x let sum a b = a + b let main { if sum(id(2), id(3)) == 5 and sum(id(2.5), id(3.5)) == 6.0 then 9 else 0 }",
            9,
        );
    }

    #[test]
    fn poly_recursive_int() {
        expect_exit(
            "let f n = if n == 0 then 0 else f(n - 1) + 1 let main { f(5) }",
            5,
        );
    }

    #[test]
    fn poly_mutual_recursion_int_float() {
        expect_exit(
            "let inci x = x + 1 let incf x = x + 1.0 let twicei x = inci(x) + inci(x) let twicef x = incf(x) + incf(x) let main { if twicei(3) == 8 and twicef(3.0) == 8.0 then 12 else 0 }",
            12,
        );
    }

    #[test]
    fn poly_chain_mixed() {
        expect_exit(
            "let inci x = x + 1 let incf x = x + 1.0 let mul x y = x * y let main { if mul(inci(4),2) == 10 and mul(incf(4.0),2.0) == 10.0 then 13 else 0 }",
            13,
        );
    }

    #[test]
    fn poly_float_recursion() {
        expect_exit(
            "let f x = if x == 0.0 then 0.0 else f(x - 1.0) + 1.0 let main { if f(5.0) == 5.0 then 1 else 0 }",
            1,
        );
    }

    #[test]
    fn poly_generic_chain_identity() {
        expect_exit(
            "let id x = x let use x = id(x) let main { if use(7) == 7 and use(7.0) == 7.0 then 1 else 0 }",
            1,
        );
    }

    #[test]
    fn struct_field_access() {
        expect_exit(
            "struct Vec { x: float, y: float, z: float } let main { let v = Vec { x: 1.0, y: 2.0, z: 3.0 } if v.x + v.y + v.z > 5.0 then 42 else 0 }",
            42,
        );
    }

    #[test]
    fn struct_nested_field_access() {
        expect_exit(
            "struct Point { x: float, y: float } struct Circle { center: Point, radius: float } let main { let c = Circle { center: Point { x: 0.0, y: 0.0 }, radius: 5.0 } if c.center.x == 0.0 and c.center.y == 0.0 then 1 else 0 }",
            1,
        );
    }

    #[test]
    fn array_int_1d() {
        expect_exit("let main { let a = [1,2,3,4] 0 }", 0);
    }

    #[test]
    fn array_int_2d() {
        expect_exit("let main { let a = [[1,2],[3,4],[5,6]] a[1][1] }", 4);
    }

    #[test]
    fn array_struct_1d() {
        expect_exit(
            "struct P { x: int, y: int } let main { let a = [ P { x:1, y:2 }, P { x:3, y:4 } ] 0 }",
            0,
        );
    }

    // Field / array accessors & initializer extreme / combination tests
    #[test]
    fn array_element_access() {
        expect_exit("let main { let a = [10,20,30,40] a[2] }", 30);
    }

    #[test]
    fn array_element_access_last() {
        expect_exit("let main { let a = [1,2,3,4,5,6,7,8] a[7] }", 8);
    }

    #[test]
    fn nested_array_element_access() {
        expect_exit("let main { let a = [[1,2],[3,4]] a[1][0] }", 3);
    }

    #[test]
    fn multi_dimensional_array_access() {
        expect_exit(
            "let main { let a = [[[1,2],[3,4]],[[5,6],[7,8]]] a[1][0][1] }",
            6,
        );
    }

    #[test]
    fn struct_with_array_field_access() {
        expect_exit(
            "struct S { xs: int[4], p: int } let main { let s = S { xs: [1,2,3,4], p: 5 } s.xs[3] + s.p }",
            9,
        );
    }

    #[test]
    fn array_of_struct_field_access() {
        expect_exit(
            "struct P { x: int, y: int } let main { let arr = [ P { x:1, y:2 }, P { x:3, y:4 } ] arr[1].y }",
            4,
        );
    }

    #[test]
    fn nested_struct_array_chain_access() {
        expect_exit(
            "struct Point { x: int, y: int } struct Row { p: Point } struct Grid { rows: Row[2] } let main { let g = Grid { rows: [ Row { p: Point { x:10, y:20 } }, Row { p: Point { x:30, y:40 } } ] } g.rows[1].p.x }",
            30,
        );
    }

    #[test]
    fn struct_with_nested_array_of_structs_access() {
        expect_exit(
            "struct P { x: int } struct Holder { ps: P[2][2] } let main { let h = Holder { ps: [ [ P { x:1 }, P { x:2 } ], [ P { x:3 }, P { x:4 } ] ] } h.ps[1][0].x }",
            3,
        );
    }

    #[test]
    fn struct_positional_init() {
        expect_exit(
            "struct Vec3 { x: float, y: float, z: float } let main { let v = Vec3 { 1.0, 2.0, 3.0 } if v.x + v.y + v.z == 6.0 then 42 else 0 }",
            42,
        );
    }

    // --- Additional elaborate accessor / initializer tests ---
    #[test]
    fn array_of_structs_multi_access() {
        expect_exit(
            "struct Pos { x: int, y: int } struct Seg { start: Pos, end: Pos } let main { let segs = [ Seg { start: Pos { 1, 2 }, end: Pos { 3, 4 } }, Seg { start: Pos { 5, 6 }, end: Pos { 7, 8 } } ] if segs[1].end.y == 8 and segs[0].start.x == 1 then 9 else 0 }",
            9,
        );
    }

    #[test]
    fn positional_init_with_function_calls() {
        expect_exit(
            "struct Vec3 { x: float, y: float, z: float } let inc a = a + 1.0 let main { let v = Vec3 { inc(0.5), inc(1.5), inc(2.5) } if v.x + v.y + v.z == 0.5+1.0+1.5+1.0+2.5+1.0 then 42 else 0 }",
            42,
        );
    }

    #[test]
    fn nested_arrays_of_structs_field_sum() {
        expect_exit(
            "struct P { x: int, y: int } let main { let grid = [ [ P { 1, 2 }, P { 3, 4 } ], [ P { 5, 6 }, P { 7, 8 } ] ] if grid[0][1].y + grid[1][0].x + grid[1][1].y == 4 + 5 + 8 then 42 else 0 }",
            42,
        );
    }

    #[test]
    fn struct_with_multi_dim_array_field_access_combo() {
        expect_exit(
            "struct M { grid: int[2][2], val: int } let main { let m = M { grid: [ [1,2], [3,4] ], val: 5 } if m.grid[0][1] + m.grid[1][0] + m.val == 2 + 3 + 5 then 42 else 0 }",
            42,
        );
    }

    #[test]
    fn function_returning_struct_positional() {
        expect_exit(
            "struct Pair { a: int, b: int } let make a b = Pair { a, b } let main { let p = make(10, 32) if p.a + p.b == 42 then 42 else 0 }",
            42,
        );
    }

    #[test]
    fn identity_function_on_struct_fields() {
        expect_exit(
            "struct V { x: int, y: int } let id a = a let main { let v = V { 10, 20 } if id(v.x) + id(v.y) == 30 then 42 else 0 }",
            42,
        );
    }

    #[test]
    fn deep_nested_small_arrays() {
        expect_exit(
            "let main { let a = [ [ [1],[2] ], [ [3],[4] ], [ [5],[6] ] ] if a[2][0][0] + a[0][1][0] == 5 + 2 then 42 else 0 }",
            42,
        );
    }

    #[test]
    fn complex_chained_array_struct_access() {
        expect_exit(
            "struct Point { x: int, y: int } struct Shape { points: Point[3], scale: int } let main { let s = Shape { points: [ Point { 1,2 }, Point { 3,4 }, Point { 5,6 } ], scale: 10 } if s.points[0].x + s.points[1].y + s.points[2].x + s.scale == 1 + 4 + 5 + 10 then 20 else 0 }",
            20,
        );
    }

    #[test]
    fn float_struct_field_mixed_ops() {
        expect_exit(
            "struct Vec3 { x: float, y: float, z: float } let main { let v = Vec3 { 1.5, 2.5, 3.0 } if (v.x + v.y) * v.z == (1.5 + 2.5) * 3.0 then 42 else 0 }",
            42,
        );
    }

    // --- Recursive / mutual recursion / struct passing tests ---
    #[test]
    fn recursive_gcd() {
        expect_exit(
            "let gcd a b = if b == 0 then a else gcd(b, a - (a / b) * b) let main { gcd(48, 18) }",
            6,
        );
    }

    #[test]
    fn recursive_power() {
        expect_exit(
            "let pow a b = if b == 0 then 1 else a * pow(a, b - 1) let main { pow(3,4) }",
            81,
        );
    }

    #[test]
    fn recursive_binomial() {
        expect_exit(
            "let bin n k = if k == 0 or k == n then 1 else bin(n-1,k-1) + bin(n-1,k) let main { bin(6,2) }",
            15,
        );
    }

    #[test]
    fn recursive_tribonacci() {
        expect_exit(
            "let tri n = if n < 3 then n else tri(n-1) + tri(n-2) + tri(n-3) let main { tri(7) }",
            37,
        );
    }

    #[test]
    fn mutual_recursion_even_odd() {
        expect_exit(
            "let even n = if n == 0 then 1 else odd(n - 1) let odd n = if n == 0 then 0 else even(n - 1) let main { if even(10) == 1 and odd(9) == 1 then 42 else 0 }",
            42,
        );
    }

    #[test]
    fn struct_param_passing() {
        expect_exit(
            "struct Pair { a: int, b: int } let sum p = p.a + p.b let make a b = Pair { a, b } let main { sum(make(19,23)) }",
            42,
        );
    }

    #[test]
    fn struct_param_nested_access() {
        expect_exit(
            "struct Inner { v: int } struct Outer { i: Inner, w: int } let use o = o.i.v + o.w let main { use(Outer { Inner { 20 }, 22 }) }",
            42,
        );
    }

    #[test]
    fn mutual_recursion_struct_counter() {
        expect_exit(
            "struct C { v: int } let dec c = if c.v == 0 then 0 else inc(C { c.v - 1 }) let inc c = if c.v == 0 then 1 else dec(C { c.v - 1 }) let main { dec(C { 5 }) + inc(C { 4 }) }",
            2,
        );
    }

    #[test]
    fn recursive_sum_array_like_literal() {
        expect_exit(
            "let sum4 a b c d = a + b + c + d let main { sum4(1,2,3,36) }",
            42,
        );
    }

    #[test]
    fn recursive_factorial_variant() {
        expect_exit(
            "let fact n = if n < 2 then 1 else n * fact(n - 1) let main { fact(7) }",
            5040,
        );
    }

    #[test]
    fn multi_dim_array_generic_sum2d() {
        expect_exit(
            "let sum2d m = m[0][0] + m[0][1] + m[1][0] + m[1][1] let main { if sum2d([[1,2],[3,4]]) == 10 and sum2d([[1.0,2.0],[3.0,4.0]]) == 10.0 then 42 else 0 }",
            42,
        );
    }

    #[test]
    fn nested_struct_multi_array_access() {
        expect_exit(
            "struct Pixel { r: int, g: int, b: int } struct Image { pixels: Pixel[2][2], scale: int } let main { let img = Image { pixels: [ [ Pixel { 1,2,3 }, Pixel { 4,5,6 } ], [ Pixel { 7,8,9 }, Pixel { 10,11,12 } ] ], scale: 2 } if img.pixels[0][1].g + img.pixels[1][0].r + img.scale == 5 + 7 + 2 then 1 else 0 }",
            1,
        );
    }

    #[test]
    fn deep_nested_struct_arrays() {
        expect_exit(
            "struct Leaf { v: int } struct Node { left: Leaf[2], right: Leaf[2] } struct Container { nodes: Node[2] } let main { let c = Container { nodes: [ Node { left: [ Leaf { 1 }, Leaf { 2 } ], right: [ Leaf { 3 }, Leaf { 4 } ] }, Node { left: [ Leaf { 5 }, Leaf { 6 } ], right: [ Leaf { 7 }, Leaf { 8 } ] } ] } if c.nodes[1].left[0].v + c.nodes[0].right[1].v == 5 + 4 then 9 else 0 }",
            9,
        );
    }

    #[test]
    fn nested_shadowing_three_levels() {
        expect_exit(
            "let main { let x = 5 { let x = x + 1 { let x = x + 2 x } } }",
            8,
        );
    }

    #[test]
    fn generic_pick_struct_fields() {
        expect_exit(
            "struct Pair { a: int, b: float } let pick x = x let main { let p = Pair { 10, 2.5 } if pick(p.a) + pick(p.a) == 20 and pick(p.b) + pick(p.b) == 5.0 then 42 else 0 }",
            42,
        );
    }

    #[test]
    fn complex_image_access() {
        expect_exit(
            "struct Point { x: int, y: int } struct Shape { points: Point[2][2], scale: int } let scale_add a b c = a + b + c let main { let s = Shape { points: [ [ Point { 1,2 }, Point { 3,4 } ], [ Point { 5,6 }, Point { 7,8 } ] ], scale: 10 } if scale_add(s.points[0][0].x, s.points[1][1].y, s.scale) == 1 + 8 + 10 then 19 else 0 }",
            19,
        );
    }
}
