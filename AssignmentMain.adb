with text_io;
with MatrixMult;
use MatrixMult;

procedure AssignmentMain is
use text_io;
package int_io is new integer_io(integer);
use int_io;

A: Matrix;
B: Matrix;
C: Matrix;

task Reader1 is
  entry R1;
end Reader1;

task Reader2 is
  entry R2;
end Reader2;

task Printer is
  entry P;
end Printer;

task body Reader1 is
  x: integer;
begin
  accept R1 do
  for i in 1..MatrixMult.Dim loop
    for j in 1..MatrixMult.Dim loop
      get(x);
      A(i, j):= x;
    end loop;
  end loop;
  Reader2.R2;
  end R1;
end Reader1;

task body Reader2 is
  y: integer;
begin
  accept R2 do
  for i in 1..MatrixMult.Dim loop
    for j in 1..MatrixMult.Dim loop
      get(y);
      B(i, j):= y;
    end loop;
  end loop;
  end R2;
end Reader2;

task body Printer is
begin
  accept P do
  for i in 1..MatrixMult.Dim loop
    for j in 1..MatrixMult.Dim loop
      put(C(i, j));
    end loop;
    New_Line;
  end loop;
  end P;
end Printer;

begin
  Reader1.R1;
  MatMult(A, B, C);
  Printer.P;
end AssignmentMain;