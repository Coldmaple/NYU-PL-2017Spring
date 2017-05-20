package MatrixMult is
   Dim: constant Integer:= 10; 
   type Matrix is array (1..Dim, 1..Dim) of Integer;
   procedure MatMult(A: in Matrix; B: in Matrix; C: out Matrix);
end MatrixMult;
