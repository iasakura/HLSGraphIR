macro_rules! to_arg {
    ( $name:ident ) => {
        crate::types::Arg::Var (crate::types::Var { name: String::from(stringify!($name)) } )
    };
    ( $lit:literal ) => {
        crate::types::Arg::Val ($lit)
    }
}

// macro_rules! to_args {
//     ( ( $( $arg:expr ),* ) ) => { ( $( to_arg!(arg) ),* ) };
// }

macro_rules! to_expr {
    ( $name:ident ( $( $arg:tt ),* ) ) => {
        crate::types::$name( $( to_arg!($arg) ),* )
    };
}

macro_rules! to_body {
    ( { $( $x:ident := $name:tt $args:tt );* } ) => {
        vec!( $( crate::types::Stmt{ var: String::from(stringify!($x)), expr: to_expr!( $name $args ) } ),* )
    };
}

macro_rules! loop_ir {
    ( 
        $name:ident( $( $param:ident ),* ) {
            init $init_body:tt
            while( $cond_name:tt $cond_args:tt ) $while_body:tt 
            exit $exit_body:tt
        }
    ) => {
        crate::types::LoopIR {
            name: String::from(stringify!($name)),
            params: vec!( $( crate::types::Var{ name: String::from(stringify!($param)) } ),* ),
            cond: to_expr!( $cond_name $cond_args ),
            init_body: to_body!( $init_body ),
            while_body: to_body!( $while_body ),
            exit_body: to_body!( $exit_body )
        }
    };
}

#[test]
fn test_to_expr() {
    // println!("{:?}", to_arg!(b))
    println!("{:?}", to_expr!(mu(b, c)));
    println!("{:?}", to_expr!(eq(cur2, 0)));
}