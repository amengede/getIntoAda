with Logger;
with Vulkan.Core;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Instances is
    procedure Initialize (Instance : in out Vulkan.Instance) is
        App_Info : aliased Vulkan.Application_Info;
        Create_Info : Vulkan.Instance_Create_Info;
        Version : constant Vulkan.Version_Number := Vulkan.Create_Version (1, 4);
        use type Vulkan.Result;
        Result : Vulkan.Result;
    begin
        
        App_Info.Application_Name := To_Unbounded_String ("Ye Newe Tourney");
        App_Info.Engine_Name      := To_Unbounded_String ("Real Engine");
        App_Info.Engine_Version   := Vulkan.Create_Version (1, 0);
        App_Info.API_Version      := Version;

        Create_Info.Application_Info := App_Info'Unchecked_Access;

        Result := Vulkan.Core.Create (Create_Info, Instance);

        if Result = Vulkan.Success then
            Logger.Print ("Created an instance!");
        else
            Logger.Print ("Failed to create an instance!");
        end if;

    end Initialize;

end Instances;
