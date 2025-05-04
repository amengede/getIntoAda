-- This file is part of VulkAda.

-- VulkAda is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as
-- published by the Free Software Foundation, either version 3 of
-- the License, or (at your option) any later version.

-- VulkAda is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.

-- You should have received a copy of the GNU Lesser General Public
-- License along with VulkAda.
-- If not, see <http://www.gnu.org/licenses/>.

-- Copyright 2024 Phaser Cat Games LLC

-- C interface for the rasterization order extension

with Vulkan.Utilities;
with Vulkan.Extension_Records;

package body Vulkan.Rasterization_Orders_C is
    function To_C(Struct: in Pipeline_Rasterization_State_Rasterization_Order)
        return Pipeline_Rasterization_State_Rasterization_Order_C is
        PRSROC: Pipeline_Rasterization_State_Rasterization_Order_C;
    begin
        PRSROC.Next := Extension_Records.To_C(Struct.Next);
        PRSROC.Rasterization_Order := Struct.Rasterization_Order;

        return PRSROC;
    end To_C;

    procedure Free
        (Struct: in out Pipeline_Rasterization_State_Rasterization_Order_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case Structure(Next.Record_Type) is
            when Pipeline_Rasterization_State_Rasterization_Order_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                    (Pipeline_Rasterization_State_Rasterization_Order,
                     Pipeline_Rasterization_State_Rasterization_Order_C,
                     Pipeline_Rasterization_State_Rasterization_Order_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure Free(Next: in out C.In_Structure_C_Access) is
        use type C.In_Structure_C_Access;
    begin
        if Next = null then
            return;
        end if;

        case Structure(Next.Record_Type) is
            when Pipeline_Rasterization_State_Rasterization_Order_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                    (Pipeline_Rasterization_State_Rasterization_Order_C,
                     Pipeline_Rasterization_State_Rasterization_Order_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.Rasterization_Orders_C;

