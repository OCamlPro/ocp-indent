[%%sig:
  module type M = sig
    val x : int
  end

  module S : module type of
  struct
    let x = 12
  end
]
