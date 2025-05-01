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

-- Pipeline related subroutines

with Vulkan.Exceptions;
with Vulkan.C;

package body Vulkan.Pipelines is
    -- Generic creation and destruction.
    generic
        with package Create_Info_Vectors is new Ada.Containers.Vectors(<>);
        type Create_Info_C is private;
        with function To_C(Info: in Create_Info_Vectors.Element_Type)
                return Create_Info_C;
        with procedure Free(Info: in out Create_Info_C);
        with function Create_Pipelines
            (Device: in Vulkan.Device;
             Pipeline_Cache: in Vulkan.Pipeline_Cache;
             Create_Info_Count: in Interfaces.Unsigned_32;
             Create_Infos: access constant Create_Info_C;
             Allocator: access constant Allocation_Callbacks;
             Pipelines: access Pipeline) return Result;
    function Create_Pipelines(Device: in Vulkan.Device;
                              Pipeline_Cache: in Vulkan.Pipeline_Cache;
                              Create_Infos: in Create_Info_Vectors.Vector;
                              Allocator: access constant Allocation_Callbacks;
                              Pipelines: in out Pipeline_Vectors.Vector)
        return Result;

    generic
        type Create_Info(<>) is limited private;
        type Create_Info_C is private;
        with function To_C(Info: in Create_Info) return Create_Info_C;
        with procedure Free(Info: in out Create_Info_C);
        with function Create_Pipelines
            (Device: in Vulkan.Device;
             Pipeline_Cache: in Vulkan.Pipeline_Cache;
             Create_Info_Count: in Interfaces.Unsigned_32;
             Create_Infos: access constant Create_Info_C;
             Allocator: access constant Allocation_Callbacks;
             Pipelines: access Pipeline) return Result;
    function Create_Single_Pipeline
        (Device: in Vulkan.Device;
         Pipeline_Cache: in Vulkan.Pipeline_Cache;
         Info: in Create_Info;
         Allocator: access constant Allocation_Callbacks;
         Pipeline: out Vulkan.Pipeline) return Result;

    function Create_Pipelines(Device: in Vulkan.Device;
                              Pipeline_Cache: in Vulkan.Pipeline_Cache;
                              Create_Infos: in Create_Info_Vectors.Vector;
                              Allocator: access constant Allocation_Callbacks;
                              Pipelines: in out Pipeline_Vectors.Vector)
        return Result is
        Create_Infos_C: array (1 .. Positive(Create_Infos.Length))
            of aliased Create_Info_C
            with Convention => C;
        Pipeline_Array: array (1 .. Create_Infos.Length) of aliased Pipeline
            with Convention => C;
        Result: Vulkan.Result;
        Index: Positive := 1;
    begin
        for Create_Info of Create_Infos loop
            Create_Infos_C(Index) := To_C(Create_Info);
            Index := Index + 1;
        end loop;

        Result := Create_Pipelines
            (Device,
             Pipeline_Cache,
             Interfaces.Unsigned_32(Create_Infos_C'Length),
             Create_Infos_C(1)'Access,
             Allocator,
             Pipeline_Array(1)'Access);

        Pipelines.Clear;

        for Pipeline of Pipeline_Array loop
            Pipelines.Append(Pipeline);
        end loop;

        for Create_Info of Create_Infos_C loop
            Free(Create_Info);
        end loop;

        return Result;
    end Create_Pipelines;

    function Create_Single_Pipeline
        (Device: in Vulkan.Device;
         Pipeline_Cache: in Vulkan.Pipeline_Cache;
         Info: in Create_Info;
         Allocator: access constant Allocation_Callbacks;
         Pipeline: out Vulkan.Pipeline) return Result is
        Info_C: aliased Create_Info_C := To_C(Info);
        Local_Pipeline: aliased Vulkan.Pipeline;
        Result: Vulkan.Result;
    begin
        Result := Create_Pipelines(Device,
                                   Pipeline_Cache,
                                   1,
                                   Info_C'Access,
                                   Allocator,
                                   Local_Pipeline'Access);
        Free(Info_C);
        Pipeline := Local_Pipeline;

        return Result;
    end Create_Single_Pipeline;

    function Create_Graphics_Pipelines is
        new Create_Pipelines(Graphics_Pipeline_Create_Info_Vectors,
                             C.Graphics_Pipeline_Create_Info_C,
                             C.To_C,
                             C.Free,
                             C.vkCreateGraphicsPipelines);

    function Create_Compute_Pipelines is
        new Create_Pipelines(Compute_Pipeline_Create_Info_Vectors,
                             C.Compute_Pipeline_Create_Info_C,
                             C.To_C,
                             C.Free,
                             C.vkCreateComputePipelines);

    function Create_Single_Graphics_Pipeline is
        new Create_Single_Pipeline(Graphics_Pipeline_Create_Info,
                                   C.Graphics_Pipeline_Create_Info_C,
                                   C.To_C,
                                   C.Free,
                                   C.vkCreateGraphicsPipelines);
    
    function Create_Single_Compute_Pipeline is
        new Create_Single_Pipeline(Compute_Pipeline_Create_Info,
                                   C.Compute_Pipeline_Create_Info_C,
                                   C.To_C,
                                   C.Free,
                                   C.vkCreateComputePipelines);

    function Create
        (Device: in Vulkan.Device;
         Pipeline_Cache: in Vulkan.Pipeline_Cache;
         Create_Infos: in Graphics_Pipeline_Create_Info_Vectors.Vector;
         Allocator: aliased in Allocation_Callbacks;
         Pipelines: in out Pipeline_Vectors.Vector) return Result is
    begin
        return Create_Graphics_Pipelines(Device,
                                         Pipeline_Cache,
                                         Create_Infos,
                                         Allocator'Access,
                                         Pipelines);
    end Create;

    function Create(Device: in Vulkan.Device;
                    Pipeline_Cache: in Vulkan.Pipeline_Cache;
                    Create_Info: in Graphics_Pipeline_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Pipeline: out Vulkan.Pipeline) return Result is
    begin
        return Create_Single_Graphics_Pipeline(Device,
                                               Pipeline_Cache,
                                               Create_Info,
                                               Allocator'Access,
                                               Pipeline);
    end Create;

    function Create
        (Device: in Vulkan.Device;
         Pipeline_Cache: in Vulkan.Pipeline_Cache;
         Create_Infos: in Graphics_Pipeline_Create_Info_Vectors.Vector;
         Pipelines: in out Pipeline_Vectors.Vector) return Result is
    begin
        return Create_Graphics_Pipelines(Device,
                                         Pipeline_Cache,
                                         Create_Infos,
                                         null,
                                         Pipelines);
    end Create;

    function Create(Device: in Vulkan.Device;
                    Pipeline_Cache: in Vulkan.Pipeline_Cache;
                    Create_Info: in Graphics_Pipeline_Create_Info;
                    Pipeline: out Vulkan.Pipeline) return Result is
    begin
        return Create_Single_Graphics_Pipeline(Device,
                                               Pipeline_Cache,
                                               Create_Info,
                                               null,
                                               Pipeline);
    end Create;

    function Create
        (Device: in Vulkan.Device;
         Pipeline_Cache: in Vulkan.Pipeline_Cache;
         Create_Infos: in Compute_Pipeline_Create_Info_Vectors.Vector;
         Allocator: aliased in Allocation_Callbacks;
         Pipelines: in out Pipeline_Vectors.Vector) return Result is
    begin
        return Create_Compute_Pipelines(Device,
                                        Pipeline_Cache,
                                        Create_Infos,
                                        Allocator'Access,
                                        Pipelines);
    end Create;

    function Create(Device: in Vulkan.Device;
                    Pipeline_Cache: in Vulkan.Pipeline_Cache;
                    Create_Info: in Compute_Pipeline_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Pipeline: out Vulkan.Pipeline) return Result is
    begin
        return Create_Single_Compute_Pipeline(Device,
                                              Pipeline_Cache,
                                              Create_Info,
                                              Allocator'Access,
                                              Pipeline);
    end Create;

    function Create
        (Device: in Vulkan.Device;
         Pipeline_Cache: in Vulkan.Pipeline_Cache;
         Create_Infos: in Compute_Pipeline_Create_Info_Vectors.Vector;
         Pipelines: in out Pipeline_Vectors.Vector) return Result is
    begin
        return Create_Compute_Pipelines(Device,
                                        Pipeline_Cache,
                                        Create_Infos,
                                        null,
                                        Pipelines);
    end Create;

    function Create(Device: in Vulkan.Device;
                    Pipeline_Cache: in Vulkan.Pipeline_Cache;
                    Create_Info: in Compute_Pipeline_Create_Info;
                    Pipeline: out Vulkan.Pipeline) return Result is
    begin
        return Create_Single_Compute_Pipeline(Device,
                                              Pipeline_Cache,
                                              Create_Info,
                                              null,
                                              Pipeline);
    end Create;

    procedure Destroy(Device: in Vulkan.Device;
                      Pipeline: in out Vulkan.Pipeline;
                      Allocator: aliased in Allocation_Callbacks) is
    begin
        C.vkDestroyPipeline(Device, Pipeline, Allocator'Access);
        Pipeline := No_Pipeline;
    end Destroy;

    procedure Destroy(Device: in Vulkan.Device;
                      Pipeline: in out Vulkan.Pipeline) is
    begin
        C.vkDestroyPipeline(Device, Pipeline, null);
        Pipeline := No_Pipeline;
    end Destroy;
end Vulkan.Pipelines;

