let cap = (['A'-'Z']+)

rule cap = parse
  | cap { () }

let ident = ['a'-'z']+

            rule ident = parse
              | ident { () }
