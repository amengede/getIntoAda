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

-- Operations for the pipeline executable properties extension

with Vulkan.Pipeline_Executable_Properties_C;
with Vulkan.Core;
with Vulkan.Exceptions;
with Vulkan.Extension_Records;

package body Vulkan.Pipeline_Executable_Properties_Ext is
    -- Loaded extension functions.
    type vkGetPipelineExecutablePropertiesKHR_Access is
        access function
            (Device: in Vulkan.Device;
             Pipeline_Info: in Pipeline_Executable_Properties_C.Pipeline_Info_C;
             Executable_Count: in out Interfaces.Unsigned_32;
             Properties: access
              Pipeline_Executable_Properties_C.Pipeline_Executable_Properties_C)
        return Result
        with Convention => C;

    vkGetPipelineExecutablePropertiesKHR:
        vkGetPipelineExecutablePropertiesKHR_Access;

    type vkGetPipelineExecutableStatisticsKHR_Access is
        access function
            (Device: in Vulkan.Device;
             Executable_Info: in
                Pipeline_Executable_Properties_C.Pipeline_Executable_Info_C;
             Statistic_Count: in out Interfaces.Unsigned_32;
             Statistics: access
              Pipeline_Executable_Properties_C.Pipeline_Executable_Statistic_C)
        return Result
        with Convention => C;

    vkGetPipelineExecutableStatisticsKHR:
        vkGetPipelineExecutableStatisticsKHR_Access;

    type vkGetPipelineExecutableInternalRepresentationsKHR_Access is
        access function
            (Device: in Vulkan.Device;
             Executable_Info: in
                Pipeline_Executable_Properties_C.Pipeline_Executable_Info_C;
             Internal_Representation_Count: in out Interfaces.Unsigned_32;
             Internal_Representations: access
              Pipeline_Executable_Properties_C.
                  Pipeline_Executable_Internal_Representation_C)
        return Result
        with Convention => C;

    vkGetPipelineExecutableInternalRepresentationsKHR:
        vkGetPipelineExecutableInternalRepresentationsKHR_Access;

    procedure Load_Extension(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is new Load_Pointer
            (vkGetPipelineExecutablePropertiesKHR_Access);
        procedure Load is new Load_Pointer
            (vkGetPipelineExecutableStatisticsKHR_Access);
        procedure Load is new Load_Pointer
            (vkGetPipelineExecutableInternalRepresentationsKHR_Access);
    begin
        Load(vkGetPipelineExecutablePropertiesKHR,
             "vkGetPipelineExecutablePropertiesKHR");
        Load(vkGetPipelineExecutableStatisticsKHR,
             "vkGetPipelineExecutableStatisticsKHR");
        Load(vkGetPipelineExecutableInternalRepresentationsKHR,
             "vkGetPipelineExecutableInternalRepresentationsKHR");
    end Load_Extension;
    
    function Properties_Count(Device: in Vulkan.Device;
                              Pipeline_Info: in Vulkan.Pipeline_Info)
        return Interfaces.Unsigned_32 is
        Count: Interfaces.Unsigned_32 := 0;
        C_Info: Pipeline_Executable_Properties_C.Pipeline_Info_C :=
            Pipeline_Executable_Properties_C.To_C(Pipeline_Info);
    begin
        Exceptions.Check(vkGetPipelineExecutablePropertiesKHR(Device,
                                                              C_Info,
                                                              Count,
                                                              null));
        Pipeline_Executable_Properties_C.Free(C_Info);

        return Count;
    end Properties_Count;
    
    function Get_Properties
        (Device: in Vulkan.Device;
         Pipeline_Info: in Vulkan.Pipeline_Info;
         Properties: in out Pipeline_Executable_Properties_Vectors.Vector)
        return Result is
    begin
        if Properties.Is_Empty then
            return Success;
        end if;

        declare
            C_Info: Pipeline_Executable_Properties_C.Pipeline_Info_C :=
                Pipeline_Executable_Properties_C.To_C(Pipeline_Info);
            Count: Interfaces.Unsigned_32 :=
                Interfaces.Unsigned_32(Properties.Length);
            C_Properties: array (1 .. Positive(Count)) of aliased
                Pipeline_Executable_Properties_C.
                    Pipeline_Executable_Properties_C;
            Result: Vulkan.Result;
        begin
            for X in C_Properties'Range loop
                C_Properties(X).Next :=
                    Extension_Records.To_C(Properties(X).Next);
            end loop;

            Result := vkGetPipelineExecutablePropertiesKHR
                (Device, C_Info, Count, C_Properties(1)'Access);
            Pipeline_Executable_Properties_C.Free(C_Info);

            if Result in Success | Incomplete then
                for X in C_Properties'Range loop
                    Pipeline_Executable_Properties_C.To_Ada(Properties(X),
                                                            C_Properties(X));
                    Extension_Records.Free(C_Properties(X).Next);
                end loop;
            end if;

            return Result;
        end;
    end Get_Properties;
    
    function Get_Properties(Device: in Vulkan.Device;
                            Pipeline_Info: in Vulkan.Pipeline_Info)
        return Pipeline_Executable_Properties_Vectors.Vector is
        Properties: Pipeline_Executable_Properties_Vectors.Vector;
        Property: Pipeline_Executable_Properties;
    begin
        Properties.Append(Property,
                          Ada.Containers.Count_Type
                              (Properties_Count(Device, Pipeline_Info)));
        Exceptions.Check(Get_Properties(Device, Pipeline_Info, Properties));

        return Properties;
    end Get_Properties;

    function Statistics_Count(Device: in Vulkan.Device;
                              Executable_Info: in Pipeline_Executable_Info)
        return Interfaces.Unsigned_32 is
        C_Info: Pipeline_Executable_Properties_C.
                    Pipeline_Executable_Info_C :=
            Pipeline_Executable_Properties_C.To_C(Executable_Info);
        Count: Interfaces.Unsigned_32 := 0;
    begin
        Exceptions.Check(vkGetPipelineExecutableStatisticsKHR(Device,
                                                              C_Info,
                                                              Count,
                                                              null));
        Pipeline_Executable_Properties_C.Free(C_Info);

        return Count;
    end Statistics_Count;

    function Get_Statistics
        (Device: in Vulkan.Device;
         Executable_Info: in Pipeline_Executable_Info;
         Statistics: in out Pipeline_Executable_Statistic_Vectors.Vector)
        return Result is
        use type Interfaces.Unsigned_32;

        Count: Interfaces.Unsigned_32 :=
            Interfaces.Unsigned_32(Statistics.Length);
    begin
        if Count = 0 then
            return Success;
        end if;

        declare
            C_Info: Pipeline_Executable_Properties_C.
                        Pipeline_Executable_Info_C :=
                    Pipeline_Executable_Properties_C.To_C(Executable_Info);
            C_Statistics: array (1 .. Positive(Count)) of aliased
                Pipeline_Executable_Properties_C.
                    Pipeline_Executable_Statistic_C(Bool32);
            Result: Vulkan.Result;
        begin
            for X in C_Statistics'Range loop
                C_Statistics(X).Next :=
                    Extension_Records.To_C(Statistics(X).Next);
            end loop;

            Result := vkGetPipelineExecutableStatisticsKHR
                (Device, C_Info, Count, C_Statistics(1)'Access);
            Pipeline_Executable_Properties_C.Free(C_Info);

            if Result in Success | Incomplete then
                Statistics.Clear;

                for CS of C_Statistics loop
                    declare
                        Stat: Pipeline_Executable_Statistic(CS.Format);
                    begin
                        Pipeline_Executable_Properties_C.To_Ada(Stat, CS);
                        Statistics.Append(Stat);
                    end;
                end loop;
            end if;

            for CS of C_Statistics loop
                Extension_Records.Free(CS.Next);
            end loop;

            return Result;
        end;
    end Get_Statistics;
    
    function Get_Statistics(Device: in Vulkan.Device;
                            Executable_Info: in Pipeline_Executable_Info)
        return Pipeline_Executable_Statistic_Vectors.Vector is
        Statistics: Pipeline_Executable_Statistic_Vectors.Vector;
    begin
        Exceptions.Check(Get_Statistics(Device, Executable_Info, Statistics));

        return Statistics;
    end Get_Statistics;

    function Internal_Representations_Count
        (Device: in Vulkan.Device;
         Executable_Info: in Pipeline_Executable_Info)
        return Interfaces.Unsigned_32 is
        Count: Interfaces.Unsigned_32 := 0;
        C_Info: Pipeline_Executable_Properties_C.Pipeline_Executable_Info_C :=
            Pipeline_Executable_Properties_C.To_C(Executable_Info);
    begin
        Exceptions.Check(vkGetPipelineExecutableInternalRepresentationsKHR
            (Device, C_Info, Count, null));
        Pipeline_Executable_Properties_C.Free(C_Info);

        return Count;
    end Internal_Representations_Count;
    
    function Get_Internal_Representations
        (Device: in Vulkan.Device;
         Executable_Info: in Pipeline_Executable_Info;
         Internal_Representations: in out
            Pipeline_Executable_Internal_Representation_Vectors.Vector)
        return Result is
    begin
        if Internal_Representations.Is_Empty then
            return Success;
        end if;

        declare
            C_Info: Pipeline_Executable_Properties_C.
                    Pipeline_Executable_Info_C :=
                Pipeline_Executable_Properties_C.To_C(Executable_Info);
            Count: Interfaces.Unsigned_32 :=
                Interfaces.Unsigned_32(Internal_Representations.Length);
            C_Representations: array (1 .. Positive(Count)) of aliased
                Pipeline_Executable_Properties_C.
                    Pipeline_Executable_Internal_Representation_C;
            Result: Vulkan.Result;
        begin
            for X in C_Representations'Range loop
                C_Representations(X).Next :=
                    Extension_Records.To_C(Internal_Representations(X).Next);
            end loop;

            Result := vkGetPipelineExecutableInternalRepresentationsKHR
                (Device, C_Info, Count, C_Representations(1)'Access);
            Pipeline_Executable_Properties_C.Free(C_Info);

            if Result in Success | Incomplete then
                for X in C_Representations'Range loop
                    Pipeline_Executable_Properties_C.To_Ada
                        (Internal_Representations(X), C_Representations(X));
                    Extension_Records.Free(C_Representations(X).Next);
                end loop;
            end if;

            return Result;
        end;
    end Get_Internal_Representations;

    function Get_Internal_Representations
        (Device: in Vulkan.Device;
         Executable_Info: in Pipeline_Executable_Info)
        return Pipeline_Executable_Internal_Representation_Vectors.Vector is
        Representations:
            Pipeline_Executable_Internal_Representation_Vectors.Vector;
        Representation: Pipeline_Executable_Internal_Representation;
    begin
        Representations.Append
            (Representation,
             Ada.Containers.Count_Type(Internal_Representations_Count
                 (Device, Executable_Info)));
        Exceptions.Check(Get_Internal_Representations(Device,
                                                      Executable_Info,
                                                      Representations));

        return Representations;
    end Get_Internal_Representations;
end Vulkan.Pipeline_Executable_Properties_Ext;

