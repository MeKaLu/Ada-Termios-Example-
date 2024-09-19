with Interfaces.C; use Interfaces.C;
with System.CRTL;
with System;

procedure Termios is
   subtype speed_t is unsigned;
   type termios is record
      c_iflag  : unsigned;
      c_oflag  : unsigned;
      c_cflag  : unsigned;
      c_lflag  : unsigned;
      c_line   : unsigned_char;
      c_cc     : Interfaces.C.char_array (0 .. 31);
      c_ispeed : speed_t;
      c_ospeed : speed_t;
   end record;
   pragma Convention (C, termios);

   function tcgetattr (fd : int; termios_p : System.Address) return int;
   pragma Import (C, tcgetattr, "tcgetattr");

   function tcsetattr
     (fd : int; action : int; termios_p : System.Address) return int;
   pragma Import (C, tcsetattr, "tcsetattr");

   procedure cfmakeraw (termios_p : System.Address);
   pragma Import (C, cfmakeraw, "cfmakeraw");

   STDIN_FILENO : constant int := 0;
   TCSAFLUSH    : constant int := 2;

   ESC_CODE     : constant Character := Character'Val (16#1B#);
   CR_CODE      : constant Character := Character'Val (16#0D#);

   Not_A_Terminal          : exception;
   Failed_To_Get_Tc_Attrib : exception;
   Failed_To_Set_Tc_Attrib : exception;
   Failed_To_Write         : exception;
   Failed_To_Read          : exception;

   type Buffer_Pos is range 1 .. 200;
   type Buffer_Str is array (Buffer_Pos) of Character 
      with Default_Component_Value => Character'Val (0);

   Saved_Termios, Current_Termios : termios;
   Buffer                         : Buffer_Str;
   Pos                            : Buffer_Pos := 1;
   Read_Char                      : Character;
begin
   if System.CRTL.IsAtty (Integer (STDIN_FILENO)) /= 1 then
      raise Not_A_Terminal with "Please use a real terminal.";
   end if;

   -- Save the terminal state so we can go back to normal
   if tcgetattr (STDIN_FILENO, Saved_Termios'Address) = -1 then
      raise Failed_To_Get_Tc_Attrib with "Failed to get terminal attributes (tcgetattr).";
   end if;

   Current_Termios := Saved_Termios;
   -- This automatically sets the given termios cfg to raw
   cfmakeraw (Current_Termios'Address);
   -- Set terminal to raw
   if tcsetattr (STDIN_FILENO, TCSAFLUSH, Current_Termios'Address) = -1 then
      raise Failed_To_Set_Tc_Attrib with "Failed to set terminal attributes (tcsetattr).";
   end if;

   while Read_Char /= ESC_CODE loop
      case Read_Char is
         when CR_CODE =>
            Buffer (Buffer_Pos'First .. Pos) := (others => ' ');
            Pos := Buffer_Pos'First;
            
            if Integer (System.CRTL.write (Integer (STDIN_FILENO), Buffer'Address, Buffer'Length)) = -1 then
               raise Failed_To_Write with "Failed to write to STDIN Buffer.";
            end if;
            
            if Integer (System.CRTL.write (Integer (STDIN_FILENO), CR_CODE'Address, 1)) = -1 then
               raise Failed_To_Write with "Failed to write to STDIN CR_CODE.";
            end if;
         when others => 
            Buffer (Pos) := Read_Char;
            Pos := (if @ = Buffer_Pos'Last then Buffer_Pos'First else @ + 1);
      
            if Integer (System.CRTL.write (Integer (STDIN_FILENO), Buffer'Address, Buffer'Length)) = -1 then
               raise Failed_To_Write with "Failed to write to STDIN Buffer.";
            end if;

            if Integer (System.CRTL.write (Integer (STDIN_FILENO), CR_CODE'Address, 1)) = -1 then
               raise Failed_To_Write with "Failed to write to STDIN CR_CODE.";
            end if;
      end case;

      if Integer (System.CRTL.read (Integer (STDIN_FILENO), Read_Char'Address, 1)) = -1 then
         raise Failed_To_Read with "Failed to read from STDIN to Read_Char.";
      end if;
   end loop;
   
   -- Reset the terminal state to normal
   if tcsetattr (STDIN_FILENO, TCSAFLUSH, Saved_Termios'Address) = -1 then
      raise Failed_To_Set_Tc_Attrib with "Failed to set terminal attributes (tcsetattr).";
   end if;

end Termios;
