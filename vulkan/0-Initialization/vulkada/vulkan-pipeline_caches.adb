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

-- Pipeline cache related subprograms

with Vulkan.Exceptions;

package body Vulkan.Pipeline_Caches is
    function Get_Available_Data_Size(Device: in Vulkan.Device;
                                     Pipeline_Cache: in Vulkan.Pipeline_Cache;
                                     Data_Size: in out Interfaces.C.size_t) return Result is
        use type Interfaces.C.size_t;

        Result: Vulkan.Result;
    begin
        Result := C.vkGetPipelineCacheData(Device,
                                           Pipeline_Cache,
                                           Data_Size,
                                           System.Null_Address);
        Data_Size := Data_Size * Interfaces.C.char_bit;

        return Result;
    end Get_Available_Data_Size;

    function Get_Data(Device: in Vulkan.Device;
                      Pipeline_Cache: in Vulkan.Pipeline_Cache;
                      Data_Size: in out Interfaces.C.size_t;
                      Data: aliased out Element_Array) return Result is
        use type Interfaces.C.size_t;

        Result: Vulkan.Result;
    begin
        Data_Size := Data_Size / Interfaces.C.char_bit;
        Result := C.vkGetPipelineCacheData(Device,
                                           Pipeline_Cache,
                                           Data_Size,
                                           Data(Data'First)'Address);
        Data_Size := Data_Size * Interfaces.C.char_bit;

        return Result;
    end Get_Data;

    function Merge(Device: in Vulkan.Device;
                   Destination_Cache: in Pipeline_Cache;
                   Source_Caches: in Pipeline_Cache_Vectors.Vector) return Result is
        Source_Cache_Array: array (1 .. Positive(Source_Caches.Length))
            of aliased Pipeline_Cache
            with Convention => C;
    begin
        for X in Source_Cache_Array'Range loop
            Source_Cache_Array(X) := Source_Caches(X);
        end loop;

        return C.vkMergePipelineCaches(Device,
                                       Destination_Cache,
                                       Interfaces.Unsigned_32(Source_Caches.Length),
                                       Source_Cache_Array(1)'Access);
    end Merge;
    
    procedure Merge(Device: in Vulkan.Device;
                    Destination_Cache: in Pipeline_Cache;
                    Source_Caches: in Pipeline_Cache_Vectors.Vector) is
    begin
        Exceptions.Check(Merge(Device, Destination_Cache, Source_Caches));
    end Merge;

    function Merge(Device: in Vulkan.Device;
                   Destination_Cache: in Pipeline_Cache;
                   Source_Cache: in Pipeline_Cache) return Result is
        Local_Cache: aliased Pipeline_Cache := Source_Cache;
    begin
        return C.vkMergePipelineCaches(Device,
                                       Destination_Cache,
                                       1,
                                       Local_Cache'Access);
    end Merge;
    
    procedure Merge(Device: in Vulkan.Device;
                    Destination_Cache: in Pipeline_Cache;
                    Source_Cache: in Pipeline_Cache) is
    begin
        Exceptions.Check(Merge(Device, Destination_Cache, Source_Cache));
    end Merge;
end Vulkan.Pipeline_Caches;

