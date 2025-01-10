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

-- Sampler YCbCr conversion subprograms

private with Vulkan.Objects_Common_Access;
private with Vulkan.C_V1_1;

package Vulkan.Sampler_YCbCr_Conversions is
    -- Vulkan 1.1
    -- vkCreateSamplerYcbcrConversion
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Sampler_YCbCr_Conversion_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    YCbCr_Conversion: out Sampler_YCbCr_Conversion)
        return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        YCbCr_Conversion /= No_Sampler_YCbCr_Conversion);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Sampler_YCbCr_Conversion_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Sampler_YCbCr_Conversion
        with Pre => Device /= No_Device,
             Post => Create'Result /= No_Sampler_YCbCr_Conversion;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Sampler_YCbCr_Conversion_Create_Info;
                    YCbCr_Conversion: out Sampler_YCbCr_Conversion)
        return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory and
                     (if Create'Result = Success then
                        YCbCr_Conversion /= No_Sampler_YCbCr_Conversion);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Sampler_YCbCr_Conversion_Create_Info)
        return Sampler_YCbCr_Conversion
        with Pre => Device /= No_Device,
             Post => Create'Result /= No_Sampler_YCbCr_Conversion;

    -- vkDestroySamplerYCbCrConversion
    procedure Destroy(Device: in Vulkan.Device;
                      YCbCr_Conversion: in out Sampler_YCbCr_Conversion;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => YCbCr_Conversion = No_Sampler_YCbCr_Conversion;

    procedure Destroy(Device: in Vulkan.Device;
                      YCbCr_Conversion: in out Sampler_YCbCr_Conversion)
        with Inline,
             Pre => Device /= No_Device,
             Post => YCbCr_Conversion = No_Sampler_YCbCr_Conversion;

private
    package Sampler_YCbCr_Conversions_Common is
        new Objects_Common_Access(Sampler_YCbCr_Conversion_Create_Info,
                                  C_V1_1.Sampler_YCbCr_Conversion_Create_Info_C,
                                  Sampler_YCbCr_Conversion,
                                  No_Sampler_YCbCr_Conversion,
                                  C_V1_1.To_C,
                                  C_V1_1.Free,
                                  C_V1_1.vkCreateSamplerYcbcrConversion_Access,
                                  C_V1_1.vkDestroySamplerYcbcrConversion_Access,
                                  C_V1_1.vkCreateSamplerYcbcrConversion,
                                  C_V1_1.vkDestroySamplerYcbcrConversion);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Sampler_YCbCr_Conversion_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    YCbCr_Conversion: out Sampler_YCbCr_Conversion)
        return Result
        renames Sampler_YCbCr_Conversions_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Sampler_YCbCr_Conversion_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
                        return Sampler_YCbCr_Conversion
        renames Sampler_YCbCr_Conversions_Common.Create;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Sampler_YCbCr_Conversion_Create_Info;
                    YCbCr_Conversion: out Sampler_YCbCr_Conversion)
        return Result
        renames Sampler_YCbCr_Conversions_Common.Create;
    
    function Create(Device: in Vulkan.Device;
                    Create_Info: in Sampler_YCbCr_Conversion_Create_Info)
        return Sampler_YCbCr_Conversion
        renames Sampler_YCbCr_Conversions_Common.Create;

    procedure Destroy(Device: in Vulkan.Device;
                      YCbCr_Conversion: in out Sampler_YCbCr_Conversion;
                      Allocator: aliased in Allocation_Callbacks)
        renames Sampler_YCbCr_Conversions_Common.Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      YCbCr_Conversion: in out Sampler_YCbCr_Conversion)
        renames Sampler_YCbCr_Conversions_Common.Destroy;
end Vulkan.Sampler_YCbCr_Conversions;

