macro_rules! to_var {
    ( $name:ident ) => { crate::types::Var { name: String::from(stringify!($name)) } }
}

macro_rules! to_arg {
    ( $name:ident ) => {
        crate::types::Arg::Var (to_var!($name))
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

// dasai...
macro_rules! to_exit {
    ( ( jc ( $cond:ident, $label1:ident, $label2:ident ) ) ) => {
        crate::types::jc(crate::types::var_from_str(stringify!($cond)), String::from(stringify!($label1)), String::from(stringify!($label2)))
    };
    ( ( jmp ( $label:ident ) ) ) => {
        crate::types::jmp(String::from(stringify!($label)))
    };
    ( ( ret ( ) )) => {
        crate::types::ret()
    };
}

macro_rules! to_bb {
    ( { prevs ( $($from_name:ident),* ) loop $bb_body:tt while ( $cond:tt ) exit $bb_exit:tt } ) => {
        crate::types::BB {
            prevs: vec![$(String::from(stringify!($from_name))),*], 
            body: crate::types::BBBody::PipeBB (to_body!($bb_body), to_var!($cond)), 
            exit: to_exit!($bb_exit)
        }
    };
    ( { prevs ( $($from_name:ident),* ) seq $bb_body:tt exit $bb_exit:tt } ) => {
        crate::types::BB {
            prevs: vec![$(String::from(stringify!($from_name))),*], 
            body: crate::types::BBBody::SeqBB (to_body!($bb_body)),
            exit: to_exit!($bb_exit)
        }
        
    };
}

macro_rules! loop_ir {
    ( 
        $name:ident( $( $param:ident ),* ) {
            starts $start_label:ident
            $( 
                $bb_name:ident $bb:tt
            ),*
            returns ( $( $ret:ident ),* )
        }
    ) => {
        crate::types::GatedSSAIR {
            name: String::from(stringify!($name)),
            start: String::from(stringify!($start_label)),
            params: vec![ $( crate::types::Var { name: String::from(stringify!($param)) } ),* ],
            cfg: vec![ $( (String::from(stringify!($bb_name)), 
                           to_bb!($bb))),* ].into_iter().collect(),
            returns: vec!( $( crate::types::Var { name: String::from(stringify!($ret)) } ),* )
        }
    };
}

#[test]
fn test_to_expr() {
    // println!("{:?}", to_arg!(b))
    // println!("{:?}", to_expr!(mu(b, c)));
    // println!("{:?}", to_expr!(eq(cur2, 0)));
}