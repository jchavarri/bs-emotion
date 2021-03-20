open Ppxlib
open Ast_helper

type contents =
  | Match of expression * case list
  | ListOf of expression
  | EmptyList

let lid name = { txt = Lident name; loc = Location.none }

let css className contents =
  Exp.apply
    (Exp.ident (lid "css"))
    [
      ( Nolabel,
        Exp.construct (lid "::")
          (Some
             (Exp.tuple
                [
                  Exp.apply
                    (Exp.ident (lid "label"))
                    (* todo: fix location? *)
                    [ Nolabel, Exp.constant (Pconst_string (className.txt, Location.none, None)) ];
                  ( match contents with
                  | Match (exp, cases) -> Exp.match_ exp cases
                  | ListOf decls -> Exp.construct (lid "::") (Some decls)
                  | EmptyList -> Exp.construct (lid "[]") None
                  );
                ])) );
    ]

class mapper =
  object (self)
    inherit Ast_traverse.map as super

    method! structure_item item =
      match item.pstr_desc with
      | Pstr_value
          ( Nonrecursive,
            [
              {
                pvb_pat = { ppat_desc = Ppat_var className };
                pvb_expr =
                  {
                    pexp_desc =
                      Pexp_extension
                        ( { txt = "css" },
                          PStr
                            [
                              {
                                pstr_desc = Pstr_eval ({ pexp_desc = Pexp_construct ({ txt = Lident "[]" }, None) }, []);
                              };
                            ] );
                  };
              };
            ] ) ->
        Str.value Nonrecursive [ Vb.mk (Pat.var className) (css className EmptyList) ]
      | Pstr_value
          ( Nonrecursive,
            [
              {
                pvb_pat = { ppat_desc = Ppat_var className };
                pvb_expr =
                  {
                    pexp_desc =
                      Pexp_extension
                        ( { txt = "css" },
                          PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_construct (_, Some exp) }, []) } ] );
                  };
              };
            ] ) ->
        Str.value Nonrecursive [ Vb.mk (Pat.var className) (css className (ListOf exp)) ]
      | Pstr_value
          ( Nonrecursive,
            [
              {
                pvb_pat = { ppat_desc = Ppat_var className };
                pvb_expr =
                  {
                    pexp_desc =
                      Pexp_fun
                        ( label,
                          optExp,
                          pat,
                          {
                            pexp_desc =
                              Pexp_extension
                                ( { txt = "css" },
                                  PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_construct (_, Some exp) }, []) } ]
                                );
                          } );
                  };
              };
            ] ) ->
        Str.value Nonrecursive [ Vb.mk (Pat.var className) (Exp.fun_ label optExp pat (css className (ListOf exp))) ]
      | Pstr_value
          ( Nonrecursive,
            [
              {
                pvb_pat = { ppat_desc = Ppat_var className };
                pvb_expr =
                  {
                    pexp_desc =
                      Pexp_fun
                        ( label1,
                          optExp1,
                          pat1,
                          {
                            pexp_desc =
                              Pexp_fun
                                ( label2,
                                  optExp2,
                                  pat2,
                                  {
                                    pexp_desc =
                                      Pexp_extension
                                        ( { txt = "css" },
                                          PStr
                                            [
                                              {
                                                pstr_desc = Pstr_eval ({ pexp_desc = Pexp_construct (_, Some exp) }, []);
                                              };
                                            ] );
                                  } );
                          } );
                  };
              };
            ] ) ->
        Str.value Nonrecursive
          [
            Vb.mk (Pat.var className)
              (Exp.fun_ label1 optExp1 pat1 (Exp.fun_ label2 optExp2 pat2 (css className (ListOf exp))));
          ]
      | Pstr_value
          ( Nonrecursive,
            [
              {
                pvb_pat = { ppat_desc = Ppat_var className };
                pvb_expr =
                  {
                    pexp_desc =
                      Pexp_fun
                        ( label,
                          optExp,
                          pat,
                          {
                            pexp_desc =
                              Pexp_extension
                                ( { txt = "css" },
                                  PStr [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_match (exp, cases) }, []) } ] );
                          } );
                  };
              };
            ] ) ->
        Str.value Nonrecursive
          [ Vb.mk (Pat.var className) (Exp.fun_ label optExp pat (css className (Match (exp, cases)))) ]
      | Pstr_value
          ( Nonrecursive,
            [
              {
                pvb_pat = { ppat_desc = Ppat_var className };
                pvb_expr =
                  {
                    pexp_desc =
                      Pexp_fun
                        ( label1,
                          optExp1,
                          pat1,
                          {
                            pexp_desc =
                              Pexp_fun
                                ( label2,
                                  optExp2,
                                  pat2,
                                  {
                                    pexp_desc =
                                      Pexp_extension
                                        ( { txt = "css" },
                                          PStr
                                            [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_match (exp, cases) }, []) } ]
                                        );
                                  } );
                          } );
                  };
              };
            ] ) ->
        Str.value Nonrecursive
          [
            Vb.mk (Pat.var className)
              (Exp.fun_ label1 optExp1 pat1 (Exp.fun_ label2 optExp2 pat2 (css className (Match (exp, cases)))));
          ]
      | _ -> super#structure_item item
  end

let structure_mapper s = (new mapper)#structure s

let () = Ppxlib.Driver.register_transformation ~preprocess_impl:structure_mapper "bs-emotion-ppx"
