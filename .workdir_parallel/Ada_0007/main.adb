with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Main is
   S : Unbounded_String;
   K : Integer;
   Min_Sub : Unbounded_String;
   Current_Sub : Unbounded_String;
   S_Str : String(1..1000);
   S_Last : Natural;
begin
   Get_Line(S_Str, S_Last);
   S := To_Unbounded_String(S_Str(1..S_Last));
   Get(K);
   
   Min_Sub := To_Unbounded_String(Slice(S, 1, K));
   
   for I in 2 .. Length(S) - K + 1 loop
      Current_Sub := To_Unbounded_String(Slice(S, I, I + K - 1));
      if Current_Sub < Min_Sub then
         Min_Sub := Current_Sub;
      end if;
   end loop;
   
   Put_Line(To_String(Min_Sub));
end Main;
