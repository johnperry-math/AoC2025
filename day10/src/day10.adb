pragma Ada_2022;

with Ada.Containers.Vectors;
with Interfaces.C;
with Ada.Text_IO;

with glpk_h;

procedure Day10 is

   Debug : constant Boolean := False;

   package IO renames Ada.Text_IO;
   package Natural_IO is new IO.Integer_IO (Num => Natural);

   package C renames Interfaces.C;
   package Glpk renames glpk_h;

   subtype Natural_Range is Natural range 0 .. 13;
   type Natural_Array_Of_Boolean is array (Natural_Range) of Boolean;
   subtype Indicator_Range is Natural range 0 .. 11;
   subtype Indicator_Array is Natural_Array_Of_Boolean;
   type Button_Array is array (Natural_Range) of Indicator_Array;
   subtype Button_Selection_Array is Natural_Array_Of_Boolean;
   type Joltage_Array is array (1 .. 12) of Natural;

   type Machine_Record is record
      Goal           : Indicator_Array;
      Buttons        : Button_Array;
      Joltages       : Joltage_Array;
      Num_Buttons    : Natural;
      Num_Indicators : Natural;
   end record;

   package Machine_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Machine_Record);
   subtype Machine_Vector is Machine_Vectors.Vector;
   Machines : Machine_Vector;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open
        (Input, IO.In_File, (if Debug then "example.txt" else "input.txt"));
      while not IO.End_Of_File (Input) loop
         declare
            Line           : constant String := IO.Get_Line (Input);
            Position       : Positive := Line'First + 1;
            Machine        : Machine_Record :=
              (Goal           => [others => False],
               Buttons        => [others => [others => False]],
               Joltages       => [others => 0],
               Num_Buttons    => 0,
               Num_Indicators => 0);
            Ith            : Natural := 0;
            Num_Indicators : Natural := 0;
         begin
            --  Goal
            while Line (Position) /= ']' loop
               Machine.Goal (Position - Line'First - 1) :=
                 Line (Position) = '#';
               Num_Indicators := @ + 1;
               Position := @ + 1;
            end loop;
            Machine.Num_Indicators := Num_Indicators;
            Position := @ + 2;
            --  Apply
            Ith := 0;
            while Line (Position) = '(' loop
               declare
                  Value : Natural;
               begin
                  while Line (Position) /= ')' loop
                     Position := @ + 1;
                     Natural_IO.Get
                       (Line (Position .. Line'Last), Value, Position);
                     Machine.Buttons (Ith) (Value) := True;
                     Position := @ + 1;
                  end loop;
               end;
               Ith := @ + 1;
               Position := @ + 2;
            end loop;
            Machine.Num_Buttons := Ith;
            --  joltages
            declare
               Joltage : Natural;
            begin
               Position := @ + 1;
               for Jth in 1 .. Num_Indicators loop
                  Natural_IO.Get
                    (Line (Position .. Line'Last), Joltage, Position);
                  Machine.Joltages (Jth) := Joltage;
                  Position := @ + 2;
               end loop;
               Machines.Append (Machine);
            end;
         end;
      end loop;
      IO.Close (Input);
   end Read_Input;

   function To_Integer (Value : Natural_Array_Of_Boolean) return Natural is
      Result : Natural := 0;
   begin
      for Ith in Indicator_Range loop
         if Value (Ith) then
            Result := @ + 2**Ith;
         end if;
      end loop;
      return Result;
   end To_Integer;

   function From_Natural (Value : Natural) return Button_Selection_Array is
      Result : Button_Selection_Array := [others => False];
      Temp   : Natural := Value;
   begin
      for Index in Natural_Range loop
         if Temp mod 2 = 1 then
            Result (Index) := True;
         end if;
         Temp := @ / 2;
      end loop;
      return Result;
   end From_Natural;

   function Press_Buttons
     (Apply   : Natural_Array_Of_Boolean;
      Machine : Machine_Record;
      Debug   : Boolean := False) return Natural
   is
      Indicator : Natural_Array_Of_Boolean := [others => False];
   begin
      if Debug then
         IO.Put_Line (Apply'Image);
      end if;
      for Ith in Natural_Range loop
         if Apply (Ith) then
            for Jth in Natural_Range loop
               if Machine.Buttons (Ith) (Jth) then
                  Indicator (Jth) := not @;
               end if;
            end loop;
         end if;
      end loop;
      return To_Integer (Indicator);
   end Press_Buttons;

   function Num_Presses (Selection : Button_Selection_Array) return Natural is
      Result : Natural := 0;
   begin
      for Each of Selection when Each loop
         Result := @ + 1;
      end loop;
      return Result;
   end Num_Presses;

   function Part_1 return Natural is
      Result             : Natural := 0;
      Machine_Number     : Natural := 0;
      Goal, Indicator    : Natural;
      Button_Selection   : Button_Selection_Array;
      Min_Button_Presses : Natural;
   begin
      for Machine of Machines loop
         Machine_Number := @ + 1;
         if Machine_Number mod 10 = 0 then
            IO.Put_Line (Machine_Number'Image & " /" & Machines.Length'Image);
         end if;
         Goal := To_Integer (Machine.Goal);
         Min_Button_Presses := Natural'Last;
         for Jth in 1 .. 2**Natural_Range'Last loop
            Button_Selection := From_Natural (Jth);
            Indicator := Press_Buttons (Button_Selection, Machine);
            if Indicator = Goal then
               Min_Button_Presses :=
                 Natural'Min
                   (Min_Button_Presses, Num_Presses (Button_Selection));
            end if;
         end loop;
         --  IO.Put_Line
         --    ("For"
         --     & Machine_Number'Image
         --     & " need only"
         --     & Min_Button_Presses'Image);
         Result := @ + Min_Button_Presses;
      end loop;
      return Result;
   end Part_1;

   Glpk_Failure : exception;

   function Solve_LP (Machine : Machine_Record) return Natural is
      Problem                 : access glpk_h.glp_prob;
      MILP_Parameters         : aliased glpk_h.glp_iocp;
      Stuff_Added, Error_Code : C.int;
      Max_Size                : constant Natural :=
        Machine.Num_Buttons * Machine.Num_Indicators + 1;
      Row_Indices             : array (1 .. Max_Size) of aliased C.int :=
        [others => 0];
      Col_Indices             : array (1 .. Max_Size) of aliased C.int :=
        [others => 0];
      Coefficients            : array (1 .. Max_Size) of aliased C.double :=
        [others => 0.0];
      Index                   : Positive := 1;
      Result                  : Natural;
   begin

      Glpk.glp_init_iocp (MILP_Parameters'Access);
      MILP_Parameters.msg_lev := Glpk.GLP_MSG_OFF;
      MILP_Parameters.presolve := Glpk.GLP_ON;

      --  set minimization problem
      Problem := Glpk.glp_create_prob;
      Glpk.glp_set_obj_dir (Problem, Glpk.GLP_MIN);

      --  set up row data (rows correspond to indicators)
      Stuff_Added :=
        Glpk.glp_add_rows (Problem, C.int (Machine.Num_Indicators));
      for Row in 1 .. Machine.Num_Indicators loop
         Glpk.glp_set_row_bnds
           (Problem,
            C.int (Row),
            Glpk.GLP_FX,
            C.double (Float (Machine.Joltages (Row))),
            C.double (Float (Machine.Joltages (Row))));
      end loop;

      --  set up col data (cols correspond to buttons)
      Stuff_Added := Glpk.glp_add_cols (Problem, C.int (Machine.Num_Buttons));
      for Col in 1 .. Machine.Num_Buttons loop
         Glpk.glp_set_obj_coef (Problem, C.int (Col), 1.0);
         Glpk.glp_set_col_kind (Problem, C.int (Col), Glpk.GLP_IV);
         Glpk.glp_set_col_bnds (Problem, C.int (Col), Glpk.GLP_LO, 0.0, 0.0);
      end loop;

      --  set up coefficient matrix
      for Ith in 0 .. Machine.Num_Buttons - 1 loop
         for Jth in
           0 .. Machine.Num_Indicators - 1
           when Machine.Buttons (Ith) (Jth)
         loop
            Coefficients (Index + 1) := 1.0;
            Row_Indices (Index + 1) := C.int (Jth + 1);
            Col_Indices (Index + 1) := C.int (Ith + 1);
            Index := @ + 1;
         end loop;
      end loop;
      Glpk.glp_load_matrix
        (Problem,
         C.int (Index - 1),
         Row_Indices (1)'Access,
         Col_Indices (1)'Access,
         Coefficients (1)'Access);

      Error_Code := Glpk.glp_intopt (Problem, MILP_Parameters'Access);
      case Error_Code is
         when 0                =>
            Result := Natural (Glpk.glp_mip_obj_val (Problem));
            Glpk.glp_delete_prob (Problem);

         when Glpk.GLP_EBOUND  =>
            raise Glpk_Failure with "Incorrect bounds";

         when Glpk.GLP_EROOT   =>
            raise Glpk_Failure
              with "Optimal basis for LP relaxation not provided";

         when Glpk.GLP_ENOPFS  =>
            raise Glpk_Failure with "No primal feasible solution";

         when Glpk.GLP_ENODFS  =>
            raise Glpk_Failure with "No dual feasible solution";

         when Glpk.GLP_EFAIL   =>
            raise Glpk_Failure with "Solver failure";

         when Glpk.GLP_EMIPGAP =>
            raise Glpk_Failure with "Relative MIP gap tolerance reached";

         when Glpk.GLP_ETMLIM  =>
            raise Glpk_Failure with "Time limit exceeded";

         when Glpk.GLP_ESTOP   =>
            raise Glpk_Failure with "Premature termination by application";

         when others           =>
            raise Glpk_Failure with "Unknown error";
      end case;

      return Result;

   end Solve_LP;

   function Part_2 return Natural is
      Result : Natural := 0;
   begin
      for Machine of Machines loop
         Result := @ + Solve_LP (Machine);
      end loop;
      return Result;
   end Part_2;

begin
   Read_Input;
   IO.Put_Line
     ("The minimum number of button presses to activate is" & Part_1'Image);
   IO.Put_Line
     ("The minimum number of button presses to attain joltage is"
      & Part_2'Image);
end Day10;
