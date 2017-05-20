with text_io;
use text_io;

package body MatrixMult is

procedure MatMult(A: in Matrix; B: in Matrix; C: out Matrix) is

task type MyTaskType is
  entry SetValue(Val1: Integer; Val2: Integer);
end MyTaskType;

task body MyTaskType is
Sum: Integer:= 0;
  begin
  accept SetValue(Val1: Integer; Val2: Integer) do
    for k in 1..Dim loop
      Sum:= Sum + A(Val1, k) * B(k, Val2);
    end loop;
    C(Val1, Val2):= Sum;
  end SetValue;
end MyTaskType;

type MyTask is access MyTaskType; 

TempTask: MyTask;

begin
  for i in 1..Dim loop
    for j in 1..Dim loop
        TempTask:= new MyTaskType; -- Need to create a new MyTaskType to realize parallel processing
        TempTask.SetValue(i, j);
      end loop;
    end loop;
end MatMult;

end MatrixMult;

