namespace malloc

module Memory =
  let mutable curAddr = 1
  let mutable freeAddresses: list<int> = []
  let malloc() = 
    match freeAddresses with
    | h::t ->
      freeAddresses <- t
      h
    | [] ->
      curAddr <- curAddr + 1
      curAddr - 1
  
  let free addr =
    freeAddresses <- addr::freeAddresses

  let rec freeList addrList =
    match addrList with
    | h::t -> free h; freeList t
    | [] -> ()
