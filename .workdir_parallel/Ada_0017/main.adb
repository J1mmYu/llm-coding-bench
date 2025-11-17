with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Main is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   
   S : Unbounded_String;
   Max_Length : Natural := 0;
begin
   S := To_Unbounded_String(Get_Line);
   
   declare
      Str : constant String := To_String(S);
      N : constant Natural := Str'Length;
   begin
      for I in Str'Range loop
         declare
            Seen : array (Character) of Boolean := (others => False);
            Length : Natural := 0;
         begin
            for J in I .. Str'Last loop
               if Seen(Str(J)) then
                  exit;
               end if;
               Seen(Str(J)) := True;
               Length := Length + 1;
            end loop;
            
            if Length > Max_Length then
               Max_Length := Length;
            end if;
         end;
      end loop;
   end;
   
   Put_Line(Natural'Image(Max_Length)(2 .. Natural'Image(Max_Length)'Last));
end Main;
