generic
    type T is private;
package Vectors is

    type Elements_Type is array (Natural range <>) of T;
    type Elements_Access is access Elements_Type;

    type Vector is
        record
            Elements: Elements_Access := new Elements_Type (0 .. 1);
            Capacity: Natural := 1;
            Size: Natural := 0;
        end record;

    type Vector_Access is access Vector;

    procedure Insert (Target: Vector_Access;
                      Index: Natural;
                      Element: T);

    procedure Append (Target: Vector_Access;
                      Element: T);

    function Get (Target: Vector_Access;
                  Index: Natural) return T;

    procedure Reserve (Target : Vector_Access;
                       New_Capacity : Natural);
end Vectors;
