with Ada.Containers.Vectors;
with Ada.Text_IO;

procedure Day01 is

   package IO renames Ada.Text_IO;

   type Turn_Enum is (Left, Right);

   type Instruction_Record is record
      Direction : Turn_Enum;
      Distance  : Positive;
   end record;

   Debug : constant Boolean := False;

   package Instruction_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Instruction_Record);
   subtype Instruction_Vector is Instruction_Vectors.Vector;
   Instructions : Instruction_Vector;

   procedure Get_Input is
      Input : IO.File_Type;
   begin
      IO.Open (Input, IO.In_File, "input.txt");
      while not IO.End_Of_File (Input) loop
         declare
            Line        : String := IO.Get_Line (Input);
            Instruction : Instruction_Record;
         begin
            if Line (1) = 'L' then
               Instruction.Direction := Left;
            else
               Instruction.Direction := Right;
            end if;
            Instruction.Distance := Positive'Value (Line (2 .. Line'Last));
            Instructions.Append (Instruction);
         end;
      end loop;
      IO.Close (Input);
   end Get_Input;

   function Part_1 return Natural is
      Result   : Natural := 0;
      Position : Integer := 50;
      Distance : Integer;
   begin
      for Instruction of Instructions loop
         Distance := Instruction.Distance;
         if Instruction.Direction = Left then
            Distance := -Distance;
         end if;
         Position := (Position + Distance) mod 100;
         if Position = 0 then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Part_1;

   function Part_2 return Natural is
      Result   : Natural := 0;
      Position : Integer := 50;
      Change   : Integer;
      Distance : Integer;
      Cycles   : Integer;
   begin
      for Instruction of Instructions loop
         Distance := Instruction.Distance;
         if Instruction.Direction = Left then
            Distance := -Distance;
         end if;
         Cycles := Distance / 100;
         Distance := Distance - Cycles * 100;
         Result := Result + abs (Cycles);
         if Debug then
            if Cycles > 0 then
               IO.Put ("Cycles: ");
               IO.Put_Line (Cycles'Image);
            end if;
            IO.Put ("Move ");
         end if;
         if Distance < 0 then
            if Debug then
               IO.Put ("Left ");
               IO.Put_Line (Distance'Image);
            end if;
            while Distance /= 0 loop
               if Position = 0 or else Distance > -Position then
                  Change := Distance;
               else
                  Change := -Position;
               end if;
               if Debug then
                  IO.Put ("   ");
                  IO.Put (Position'Image);
                  IO.Put (' ');
                  IO.Put (Distance'Image);
                  IO.Put (' ');
                  IO.Put (Change'Image);
                  IO.Put (" -> ");
               end if;
               Position := Position + Change;
               Distance := Distance - Change;
               if Position < 0 then
                  Position := Position + 100;
               end if;
               if Position = 0 then
                  Result := Result + 1;
               end if;
               if Debug then
                  IO.Put (Position'Image);
                  IO.Put (' ');
                  IO.Put (Distance'Image);
                  IO.Put (' ');
                  IO.Put_Line (Result'Image);
               end if;
            end loop;
         else
            if Debug then
               IO.Put ("Right ");
               IO.Put_Line (Distance'Image);
            end if;
            while Distance /= 0 loop
               if Position = 0 or else Distance < 100 - Position then
                  Change := Distance;
               else
                  Change := 100 - Position;
               end if;
               if Debug then
                  IO.Put ("   ");
                  IO.Put (Position'Image);
                  IO.Put (' ');
                  IO.Put (Distance'Image);
                  IO.Put (' ');
                  IO.Put (Change'Image);
                  IO.Put (" -> ");
               end if;
               Position := Position + Change;
               Distance := Distance - Change;
               if Position > 99 then
                  Position := Position - 100;
               end if;
               if Position = 0 then
                  Result := Result + 1;
               end if;
               if Debug then
                  IO.Put (Position'Image);
                  IO.Put (' ');
                  IO.Put (Distance'Image);
                  IO.Put (' ');
                  IO.Put_Line (Result'Image);
               end if;
            end loop;
         end if;
      end loop;
      return Result;
   end Part_2;

begin
   Get_Input;
   IO.Put ("The password is ");
   IO.Put_Line (Part_1'Image);
   IO.Put ("No; it is ");
   IO.Put_Line (Part_2'Image);
end Day01;
