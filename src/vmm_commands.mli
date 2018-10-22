val handle :
  [< `Console_cmd of 'a
   | `Log_cmd of 'b
   | `Policy_cmd of 'c
   | `Stats_cmd of 'd
   | `Vm_cmd of 'e ] ->
  [> `Console | `Log | `Stats | `Vmmd ] * [> `End | `Read ]
