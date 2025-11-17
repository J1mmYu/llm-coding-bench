with Ada.Text_IO;
with Ada.Containers.Ordered_Maps;

procedure Solution is
   package IO renames Ada.Text_IO;
   package Long_IO is new IO.Integer_IO(Long_Long_Integer);
   
   package Sum_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Long_Long_Integer,
      Element_Type => Integer);
   
   N : Integer;
   Sum : Long_Long_Integer := 0;
   Max_Length : Integer := 0;
   Map : Sum_Maps.Map;
   Val : Long_Long_Integer;
   
begin
   Long_IO.Get(N);
   
   -- Insert sum 0 at position 0
   Map.Insert(0, 0);
   
   for I in 1 .. N loop
      Long_IO.Get(Val);
      Sum := Sum + Val;
      
      if Map.Contains(Sum) then
         declare
            Prev_Pos : Integer := Map.Element(Sum);
            Length : Integer := I - Prev_Pos;
         begin
            if Length > Max_Length then
               Max_Length := Length;
            end if;
         end;
      else
         Map.Insert(Sum, I);
      end if;
   end loop;
   
   Long_IO.Put(Long_Long_Integer(Max_Length), Width => 0);
   IO.New_Line;
end Solution;
