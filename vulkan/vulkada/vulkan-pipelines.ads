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

-- Pipeline related subroutines

package Vulkan.Pipelines is
    use type Ada.Containers.Count_Type;

    -- vkCreateGraphicsPipelines
    function Create
        (Device: in Vulkan.Device;
         Pipeline_Cache: in Vulkan.Pipeline_Cache;
         Create_Infos: in Graphics_Pipeline_Create_Info_Vectors.Vector;
         Allocator: aliased in Allocation_Callbacks;
         Pipelines: in out Pipeline_Vectors.Vector) return Result
        with Pre => Device /= No_Device and
                    not Create_Infos.Is_Empty and
                    (for all Create_Info of Create_Infos =>
                        Create_Info.Layout /= No_Pipeline_Layout and
                        Create_Info.Render_Pass /= No_Render_Pass),
             Post => Create'Result in Success |
                                      Pipeline_Compile_Required |
                                      Out_Of_Host_Memory |
                                      Out_of_Device_Memory |
                                      Invalid_Shader and
                     (if Create'Result = Success then
                        (for all Pipeline of Pipelines =>
                            Pipeline /= No_Pipeline));

    function Create(Device: in Vulkan.Device;
                    Pipeline_Cache: in Vulkan.Pipeline_Cache;
                    Create_Info: in Graphics_Pipeline_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Pipeline: out Vulkan.Pipeline) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Layout /= No_Pipeline_Layout and
                    Create_Info.Render_Pass /= No_Render_Pass,
             Post => Create'Result in Success |
                                      Pipeline_Compile_Required |
                                      Out_Of_Host_Memory |
                                      Out_of_Device_Memory |
                                      Invalid_Shader and
                     (if Create'Result = Success then Pipeline /= No_Pipeline);

    function Create
        (Device: in Vulkan.Device;
         Pipeline_Cache: in Vulkan.Pipeline_Cache;
         Create_Infos: in Graphics_Pipeline_Create_Info_Vectors.Vector;
         Pipelines: in out Pipeline_Vectors.Vector) return Result
        with Pre => Device /= No_Device and
                    not Create_Infos.Is_Empty and
                    (for all Create_Info of Create_Infos =>
                        Create_Info.Layout /= No_Pipeline_Layout and
                        Create_Info.Render_Pass /= No_Render_Pass),
             Post => Create'Result in Success |
                                      Pipeline_Compile_Required |
                                      Out_Of_Host_Memory |
                                      Out_of_Device_Memory |
                                      Invalid_Shader and
                     (if Create'Result = Success then
                        (for all Pipeline of Pipelines =>
                            Pipeline /= No_Pipeline));

    function Create(Device: in Vulkan.Device;
                    Pipeline_Cache: in Vulkan.Pipeline_Cache;
                    Create_Info: in Graphics_Pipeline_Create_Info;
                    Pipeline: out Vulkan.Pipeline) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Layout /= No_Pipeline_Layout and
                    Create_Info.Render_Pass /= No_Render_Pass,
             Post => Create'Result in Success |
                                      Pipeline_Compile_Required |
                                      Out_Of_Host_Memory |
                                      Out_of_Device_Memory |
                                      Invalid_Shader and
                     (if Create'Result = Success then Pipeline /= No_Pipeline);

    -- vkCreateComputePipelines
    function Create
        (Device: in Vulkan.Device;
         Pipeline_Cache: in Vulkan.Pipeline_Cache;
         Create_Infos: in Compute_Pipeline_Create_Info_Vectors.Vector;
         Allocator: aliased in Allocation_Callbacks;
         Pipelines: in out Pipeline_Vectors.Vector) return Result
        with Pre => Device /= No_Device and
                    not Create_Infos.Is_Empty and
                    (for all Info of Create_Infos =>
                        Info.Stage.Stage = Shader_Stage_Compute_Bit),
             Post => Create'Result in Success |
                                      Pipeline_Compile_Required |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Invalid_Shader and
                     (if Create'Result = Success then
                        (for all Pipeline of Pipelines =>
                            Pipeline /= No_Pipeline));

    function Create(Device: in Vulkan.Device;
                    Pipeline_Cache: in Vulkan.Pipeline_Cache;
                    Create_Info: in Compute_Pipeline_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Pipeline: out Vulkan.Pipeline) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Stage.Stage = Shader_Stage_Compute_Bit,
             Post => Create'Result in Success |
                                      Pipeline_Compile_Required |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Invalid_Shader and
                     (if Create'Result = Success then Pipeline /= No_Pipeline);

    function Create
        (Device: in Vulkan.Device;
         Pipeline_Cache: in Vulkan.Pipeline_Cache;
         Create_Infos: in Compute_Pipeline_Create_Info_Vectors.Vector;
         Pipelines: in out Pipeline_Vectors.Vector) return Result
        with Pre => Device /= No_Device and
                    not Create_Infos.Is_Empty and
                    (for all Info of Create_Infos =>
                        Info.Stage.Stage = Shader_Stage_Compute_Bit),
             Post => Create'Result in Success |
                                      Pipeline_Compile_Required |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Invalid_Shader and
                     (if Create'Result = Success then
                        (for all Pipeline of Pipelines =>
                            Pipeline /= No_Pipeline));
                                                                           
    function Create(Device: in Vulkan.Device;                              
                    Pipeline_Cache: in Vulkan.Pipeline_Cache;
                    Create_Info: in Compute_Pipeline_Create_Info;
                    Pipeline: out Vulkan.Pipeline) return Result
        with Pre => Device /= No_Device and
                    Create_Info.Stage.Stage = Shader_Stage_Compute_Bit,
             Post => Create'Result in Success |
                                      Pipeline_Compile_Required |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Invalid_Shader and
                     (if Create'Result = Success then Pipeline /= No_Pipeline);

    -- vkDestroyPipeline
    procedure Destroy(Device: in Vulkan.Device;
                      Pipeline: in out Vulkan.Pipeline;
                      Allocator: aliased in Allocation_Callbacks)
        with Inline,
             Pre => Device /= No_Device,
             Post => Pipeline = No_Pipeline;

    procedure Destroy(Device: in Vulkan.Device;
                      Pipeline: in out Vulkan.Pipeline)
        with Inline,
             Pre => Device /= No_Device,
             Post => Pipeline = No_Pipeline;
end Vulkan.Pipelines;

