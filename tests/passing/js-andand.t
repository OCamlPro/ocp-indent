
  $ cat > js-andand.ml << "EOF"
  > let all_equal =
  >   a = b
  >   && c = d
  >   && e = f (* this && should line up with previous one *)
  > ;;
  > 
  > (* '=' seems to be relevant here *)
  > let _ =
  >   x
  >   && t.entity = entity
  >   && t.clearing_firm = clearing_firm
  >   && t.type_ = type_
  > EOF

  $ ocp-indent -c JaneStreet js-andand.ml
  let all_equal =
    a = b
    && c = d
    && e = f (* this && should line up with previous one *)
  ;;
  
  (* '=' seems to be relevant here *)
  let _ =
    x
    && t.entity = entity
    && t.clearing_firm = clearing_firm
    && t.type_ = type_
