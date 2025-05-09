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

-- Copyright 2025 Phaser Cat Games LLC

-- Basic Vulkan types and constants

package body Vulkan is
    function Create_Version(Major: in Major_Version;
                            Minor: in Minor_Version;
                            Patch: in Patch_Version := 0;
                            Variant: in Vulkan.Variant := 0)
        return Version_Number is
        use type Interfaces.Unsigned_32;
    begin
        return Version_Number
            (Interfaces.Shift_Left(Interfaces.Unsigned_32(Variant), 29) or
             Interfaces.Shift_Left(Interfaces.Unsigned_32(Major), 22) or
             Interfaces.Shift_Left(Interfaces.Unsigned_32(Minor), 12) or
             Interfaces.Unsigned_32(Patch));
    end Create_Version;

    function Get_Major_Version(Version: in Version_Number)
        return Major_Version is
        use type Interfaces.Unsigned_32;
    begin
        return Major_Version
            (Interfaces.Shift_Right(Interfaces.Unsigned_32(Version), 22) and
             16#7f#);
    end Get_Major_Version;

    function Get_Minor_Version(Version: in Version_Number)
        return Minor_Version is
        use type Interfaces.Unsigned_32;
    begin
        return Minor_Version
            (Interfaces.Shift_Right(Interfaces.Unsigned_32(Version), 12) and
             16#3ff#);
    end Get_Minor_Version;

    function Get_Patch_Version(Version: in Version_Number)
        return Patch_Version is
        use type Interfaces.Unsigned_32;
    begin
        return Patch_Version(Interfaces.Unsigned_32(Version) and 16#fff#);
    end Get_Patch_Version;

    function Get_Variant(Version: in Version_Number) return Variant is
    begin
        return Variant
            (Interfaces.Shift_Right(Interfaces.Unsigned_32(Version), 29));
    end Get_Variant;

    function "=" (Left, Right: in Version_Number) return Boolean is
    begin
        return Get_Major_Version(Left) = Get_Major_Version(Right) and
               Get_Minor_Version(Left) = Get_Minor_Version(Right) and
               Get_Patch_Version(Left) = Get_Patch_Version(Right);
    end "=";

    function "<" (Left, Right: in Version_Number) return Boolean is
    begin
        if Get_Major_Version(Left) /= Get_Major_Version(Right) then
            return Get_Major_Version(Left) < Get_Major_Version(Right);
        end if;

        if Get_Minor_Version(Left) /= Get_Minor_Version(Right) then
            return Get_Minor_Version(Left) < Get_Minor_Version(Right);
        end if;

        return Get_Patch_Version(Left) < Get_Patch_Version(Right);
    end "<";

    function "<=" (Left, Right: in Version_Number) return Boolean is
    begin
        return Left < Right or Left = Right;
    end "<=";

    function ">" (Left, Right: in Version_Number) return Boolean is
    begin
        return not (Left <= Right);
    end ">";

    function ">=" (Left, Right: in Version_Number) return Boolean is
    begin
        return not (Left < Right);
    end ">=";
end Vulkan;

