// Adds a and b, registering the sum.
module Adder (
  input  logic       clock,
  input  logic [7:0] a,
  input  logic [7:0] b,
  output logic [7:0] sum
);
  always_ff @(posedge clock) sum <= a + b;
endmodule
