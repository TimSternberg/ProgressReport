// Gate level model of MIPS - single cycle implementation, R-types and addi
// Authors: Tim Sternberg and Kemoy McEachron
// Date: 3/8/18


// 16-bit MIPS ALU in Verilog
module ALU (op,a,b,result,zero);
   input [15:0] a; // 16 bit values beind computed (a&b)
   input [15:0] b;
   input [2:0] op; // opcode is 3 bits
   output [15:0] result; // 16 bit a and b computed to a 16 bit result
   output zero; // zero of course can just be 1 bit
   wire c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16; // 16 wires, one for each bit

   // Implementation of a 16bit ALU
   ALU1   alu0  (a[0], b[0], op[2], op[1:0],set,   op[2],c1, result[0]),
          alu1  (a[1], b[1], op[2], op[1:0],1'b0,  c1,   c2, result[1]),
          alu2  (a[2], b[2], op[2], op[1:0],1'b0,  c2,   c3, result[2]),
          alu3  (a[3], b[3], op[2], op[1:0],1'b0,  c3,   c4, result[3]),
          alu4  (a[4], b[4], op[2], op[1:0],1'b0,  c4,   c5, result[4]),
          alu5  (a[5], b[5], op[2], op[1:0],1'b0,  c5,   c6, result[5]),
          alu6  (a[6], b[6], op[2], op[1:0],1'b0,  c6,   c7, result[6]),
          alu7  (a[7], b[7], op[2], op[1:0],1'b0,  c7,   c8, result[7]),
          alu8  (a[8], b[8], op[2], op[1:0],1'b0,  c8,   c9, result[8]),
          alu9  (a[9], b[9], op[2], op[1:0],1'b0,  c9,   c10,result[9]),
          alu10 (a[10],b[10],op[2], op[1:0],1'b0,  c10,  c11,result[10]),
          alu11 (a[11],b[11],op[2], op[1:0],1'b0,  c11,  c12,result[11]),
          alu12 (a[12],b[12],op[2], op[1:0],1'b0,  c12,  c13,result[12]),
          alu13 (a[13],b[13],op[2], op[1:0],1'b0,  c13,  c14,result[13]),
          alu14 (a[14],b[14],op[2], op[1:0],1'b0,  c14,  c15,result[14]);
   ALUmsb alu15 (a[15],b[15],op[2], op[1:0],1'b0,  c15,  c16,result[15],set);

   or or1(or01, result[0],result[1]);
   or or2(or23, result[2],result[3]);
   nor nor1(zero,or01,or23);

endmodule

// 1-bit ALU for bits 0-2
module ALU1 (a,b,binvert,op,less,carryin,carryout,result);
   input a,b,less,carryin,binvert;
   input [1:0] op;
   output carryout,result;
   wire sum, a_and_b, a_or_b, b_inv;
	
   not not1(b_inv, b);
   mux2x1 mux1(b,b_inv,binvert,b1);
   and and1(a_and_b, a, b);
   or or1(a_or_b, a, b);
   fulladder adder1(sum,carryout,a,b1,carryin);
   mux_4x1 mux2(a_and_b,a_or_b,sum,less,op[1:0],result); 

endmodule

// 1-bit ALU for the most significant bit
module ALUmsb (a,b,binvert,op,less,carryin,carryout,result,sum);
   input a,b,less,carryin,binvert;
   input [1:0] op;
   output carryout,result,sum;
   wire sum, a_and_b, a_or_b, b_inv;
	
   not not1(b_inv, b);
   mux2x1 mux1(b,b_inv,binvert,b1);
   and and1(a_and_b, a, b);
   or or1(a_or_b, a, b);
   fulladder adder1(sum,carryout,a,b1,carryin);
   mux_4x1 mux2(a_and_b,a_or_b,sum,less,op[1:0],result); 

endmodule

// implementing 4 16 bit registers
module reg_file (rr1,rr2,wr,wd,regwrite,rd1,rd2,clock);
   input [1:0] rr1,rr2,wr;
   input [15:0] wd;
   input regwrite,clock;
   output [15:0] rd1,rd2;
   wire [15:0] q1,q2,q3;

// registers
   D_flip_flop_16b r1 (wd,c1,q1),
                   r2 (wd,c2,q2),
                   r3 (wd,c3,q3);

// output port
   mux_4x1_16b  mux1 (16'b0,q1,q2,q3,rr1,rd1),
                mux2 (16'b0,q1,q2,q3,rr2,rd2);

// input port
   decoder dec(wr[1],wr[0],w3,w2,w1,w0);

   and a (regwrite_and_clock,regwrite,clock);

   and a1 (c1,regwrite_and_clock,w1),
       a2 (c2,regwrite_and_clock,w2),
       a3 (c3,regwrite_and_clock,w3);
endmodule

// D flip flop D_Latch
module D_latch(D,C,Q);
   input D,C; 
   output Q;
   wire x,y,D1,Q1; 
   nand nand1 (x,D, C), 
        nand2 (y,D1,C), 
        nand3 (Q,x,Q1),
        nand4 (Q1,y,Q); 
   not  not1  (D1,D);
endmodule 

// 1 bit d flip flop
module D_flip_flop(D,CLK,Q);
   input D,CLK; 
   output Q; 
   wire CLK1, Y;
   not  not1 (CLK1,CLK);
   D_latch D1(D,CLK, Y),
           D2(Y,CLK1,Q);
endmodule 

// implementation of a 16bit d flip flop
module D_flip_flop_16b(D,CLK,Q);
	input CLK;
	input [15:0] D; 
	output [15:0] Q; 
	genvar i; // increment value. must be declared as genvar
	generate // must use "generate" and put our for loop inside that in order to have it at all in a module
		for(i=0; i<=15; i=i+1) begin : d_loop
			D_flip_flop r(D[i],CLK,Q[i]); // implement 16 d flip flops with a for loop
		end // end for loop
	endgenerate // end generate block
endmodule


// 1bit 4x1 mux implementation
module mux_4x1(i0,i1,i2,i3,select,out);
	input i0,i1,i2,i3; // four inputs
	input [1:0] select; // select is 2 bits, since there's 4 inputs
	output out; // one output
	not n(ns0,select[0]),
		n(ns1,select[1]);
	
	and a0(q0,ns0,ns1,i0),
		a1(q1,select[0],ns1,i1),
		a2(q2,ns0,select[1],i2),
		a3(q3,select[0],select[1],i3);
	
	or o(out,q0,q1,q2,q3);
endmodule

// 16bit 4x1 mux implementation
module mux_4x1_16b(i0,i1,i2,i3,select,out);
	input [15:0] i0,i1,i2,i3; // four inputs
	input [1:0] select; // select is 2 bits, since there's 4 inputs
	output [15:0] out; // one output
	
	genvar i; // increment value. must be declared as genvar
	generate // must use "generate" and put our for loop inside that in order to have it at all in a module
		for(i=0; i<=15; i=i+1) begin : m_loop
			mux_4x1 mux(i0[i],i1[i],i2[i],i3[i],select,out[i]); // implement 16 multiplexors with a for loop
		end // end for loop
	endgenerate // end generate block
endmodule

module mux2x1(A,B,select,out); 
   input A,B,select; 
   output out; 
   wire n_s,a,b;
   not n (n_s,select);
   and a1 (a,n_s,A),
       a2 (b,select,B);
   or o (out,a,b);   
endmodule 

module decoder (S1,S0,D3,D2,D1,D0); 
   input S0,S1; 
   output D0,D1,D2,D3; 
 
   not n1 (notS0,S0),
       n2 (notS1,S1);

   and a0 (D0,notS1,notS0), 
       a1 (D1,notS1,   S0), 
       a2 (D2,   S1,notS0), 
       a3 (D3,   S1,   S0); 
endmodule 

module halfadder (S,C,x,y); 
   input x,y; 
   output S,C; 
   xor (S,x,y); 
   and (C,x,y); 
endmodule 

module fulladder (S,C,x,y,z); 
   input x,y,z; 
   output S,C; 
   wire S1,D1,D2;

   halfadder HA1 (S1,D1,x,y), 
             HA2 (S,D2,S1,z); 
   or g1(C,D2,D1); 
endmodule 

// behavioral implementation for what controls which instruction to use
module MainControl (Op,Control); 

  input [3:0] Op; // 4 bit opcode
  output reg [5:0] Control; // 6 bits

  always @(Op) case (Op)
    4'b0000: Control <= 6'b101010; // add
    4'b0001: Control <= 6'b101110; // sub
    4'b0010: Control <= 6'b101000; // and
    4'b0011: Control <= 6'b101001; // or
    4'b0111: Control <= 6'b101111; // slt
    4'b0100: Control <= 6'b011010; // addi
  endcase

endmodule

module CPU (clock,ALUOut,IR);

  input clock;
  output [15:0] ALUOut,IR;
  reg[15:0] PC;
  reg[15:0] IMemory[0:1023];
  wire [15:0] IR,NextPC,A,B,ALUOut,RD2,SignExtend;
  wire [2:0] ALUctl;
  wire [1:0] WR; 

  // our "test" program, converted from MIPS to verilog
  initial begin 
	IMemory[0] = 16'b0100_00_01_00001111;	// addi $t1, $0,  15   ($t1=15)
	IMemory[1] = 16'b0100_00_10_00000111;	// addi $t2, $0,  7    ($t2= 7)
	IMemory[2] = 16'b0010_01_10_11_000000;	// and  $t3, $t1, $t2  ($t3= 7)
	IMemory[3] = 16'b0001_01_11_10_000000;	// sub  $t2, $t1, $t3  ($t2= 8)
	IMemory[4] = 16'b0011_10_11_10_000000;	// or   $t2, $t2, $t3  ($t2=15)
	IMemory[5] = 16'b0000_10_11_11_000000;	// add  $t3, $t2, $t3  ($t3=22)
	IMemory[6] = 16'b0111_11_10_01_000000;	// slt  $t1, $t3, $t2  ($t1= 1)
	IMemory[7] = 16'b0111_10_11_01_000000;	// slt  $t1, $t2, $t3  ($t1= 0)
  end

  initial PC = 0;

  assign IR = IMemory[PC>>2];

  assign WR = (RegDst) ? IR[7:6]: IR[9:8]; // RegDst Mux

  assign B  = (ALUSrc) ? SignExtend: RD2; // ALUSrc Mux 

  assign SignExtend = {{8{IR[7]}},IR[7:0]}; // sign extension unit 8->16

  reg_file rf (IR[11:10],IR[9:8],WR,ALUOut,RegWrite,A,RD2,clock); // sending in the registers to be used

  ALU fetch (3'b010,PC,16'b0100,NextPC,Unused);

  ALU ex (ALUctl, A, B, ALUOut, Zero);

  MainControl MainCtr (IR[15:12],{RegDst,ALUSrc,RegWrite,ALUctl}); 

  always @(negedge clock) begin 
    PC <= NextPC;
  end

endmodule


// Test module
module test ();

  reg clock;
  wire [15:0] WD,IR;

  CPU test_cpu(clock,WD,IR);

  always #1 clock = ~clock;
  
  initial begin
    $display ("time clock IR       WD"); // headers
    $monitor ("%2d   %b     %b %b", $time,clock,IR,WD); // values
    clock = 1;
    #14 $finish;
  end

endmodule