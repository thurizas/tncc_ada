with GNAT.Command_Line;           use GNAT.Command_Line;
with Ada.Text_IO;                 use Ada.Text_IO;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;    use Ada.Text_IO.Unbounded_IO;
with Interfaces;                  use Interfaces;



package lexer is
    subtype Byte is Unsigned_8;                                            -- flags is an 8-bit unsigned integer
    package Byte_IO is new Ada.Text_IO.Modular_IO(Byte);

    F: File_Type;

    procedure init(inputFile:Unbounded_string; flags:Byte);

end lexer;