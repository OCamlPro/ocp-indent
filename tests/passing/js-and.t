
  $ cat > js-and.ml << "EOF"
  > module M : S with type a = b
  >               and type c = d
  >               and type e = f
  > ;;
  > EOF

  $ ocp-indent -c JaneStreet js-and.ml
  module M : S with type a = b
                and type c = d
                and type e = f
  ;;
