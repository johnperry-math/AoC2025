with HAT;

procedure Day01 is

   subtype VString is HAT.VString;

   type Turn_Enum is (Left, Right);

   type Instruction is record
      Direction : Turn_Enum;
      Distance  : Positive;
   end record;

   Length : constant Positive := 4659;
   Debug  : constant Boolean := True;

   --  miss ability to use Length as endpoint of array
   Values : array (1 .. 4659) of Instruction;

   procedure Get_Input is
      Input : HAT.File_Type;
      Line  : VString;
   begin
      HAT.Open (Input, "input.txt");
      for Ith in Values'Range loop
         HAT.Get_Line (Input, Line);
         Line := HAT.Trim_Both (Line);
         if HAT.Starts_With (Line, "L") then
            --  miss indexing for VString
            Values (Ith).Direction := Left;
         else
            Values (Ith).Direction := Right;
         end if;
         Values (Ith).Distance :=
           HAT.Integer_Value (HAT.Slice (Line, 2, HAT.Length (Line)));
      end loop;
      HAT.Close (Input);
   end Get_Input;

   function Part_1 return Natural is
      Result   : Natural := 0;
      --  miss integers modulo n
      Position : Integer := 50;
      Distance : Integer;
   begin
      for Ith in Values'Range loop
         --  miss of-style iteration
         Distance := Values (Ith).Distance;
         if Values (Ith).Direction = Left then
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
      for Ith in Values'Range loop
         Distance := Values (Ith).Distance;
         if Values (Ith).Direction = Left then
            Distance := -Distance;
         end if;
         Cycles := Distance / 100;
         Distance := Distance - Cycles * 100;
         Result := Result + abs (Cycles);
         if Cycles > 0 then
            HAT.Put ("Cycles: ");
            HAT.Put_Line (HAT.Image (Cycles));
         end if;
         if Debug then
            HAT.Put ("Move ");
         end if;
         if Distance < 0 then
            if Debug then
               HAT.Put ("Left ");
               HAT.Put_Line (Hat.Image (-Distance));
            end if;
            while Distance /= 0 loop
               --  miss Max attribute of integer
               if Position = 0 or else Distance > -Position then
                  Change := Distance;
               else
                  Change := -Position;
               end if;
               if Debug then
                  HAT.Put ("   ");
                  HAT.Put (HAT.Image (Position));
                  HAT.Put (' ');
                  HAT.Put (HAT.Image (Distance));
                  HAT.Put (' ');
                  HAT.Put (HAT.Image (Change));
                  HAT.Put (" -> ");
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
                  HAT.Put (HAT.Image (Position));
                  HAT.Put (' ');
                  HAT.Put (HAT.Image (Distance));
                  HAT.Put (' ');
                  HAT.Put_Line (Hat.Image (Result));
               end if;
            end loop;
         else
            if Debug then
               HAT.Put ("Right ");
               HAT.Put_Line (Hat.Image (Distance));
            end if;
            while Distance /= 0 loop
               if Position = 0 or else Distance < 100 - Position then
                  Change := Distance;
               else
                  Change := 100 - Position;
               end if;
               if Debug then
                  HAT.Put ("   ");
                  HAT.Put (HAT.Image (Position));
                  HAT.Put (' ');
                  HAT.Put (HAT.Image (Distance));
                  HAT.Put (' ');
                  HAT.Put (HAT.Image (Change));
                  HAT.Put (" -> ");
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
                  HAT.Put (HAT.Image (Position));
                  HAT.Put (' ');
                  HAT.Put (HAT.Image (Distance));
                  HAT.Put (' ');
                  HAT.Put_Line (Hat.Image (Result));
               end if;
            end loop;
         end if;
      end loop;
      return Result;
   end Part_2;

begin
   Get_Input;
   --  miss Image attribute (but not that much)
   --  miss immediate use of "&"
   HAT.Put ("The password is ");
   HAT.Put_Line (HAT.Image (Part_1));
   HAT.Put ("No; it is ");
   HAT.Put_Line (HAT.Image (Part_2));
--  2573 too low
end Day01;
