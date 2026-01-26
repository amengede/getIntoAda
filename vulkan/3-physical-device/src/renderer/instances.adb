with Logger;
with Vulkan.Core;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Instances is

    ---------------------------------------------------------------------------
    --  Check whether a extensions and layers are supported
    ---------------------------------------------------------------------------
    function Supported (
        Requested_Extensions : Vulkan.String_Vectors.Vector;
        Requested_Layers : Vulkan.String_Vectors.Vector)
        return Boolean is

        Supported_Extensions :
            constant Vulkan.Extension_Properties_Vectors.Vector
                := Vulkan.Core.Enumerate_Instance_Extension_Properties;

        Supported_Layers :
            constant Vulkan.Layer_Properties_Vectors.Vector
                := Vulkan.Core.Enumerate_Instance_Layer_Properties;

        Found : Boolean;
    begin
        Logger.Print("---- Supported Extensions ----");
        Logger.Print(Supported_Extensions);

        for Name of Requested_Extensions loop
            Found := False;

            for Extension of Supported_Extensions loop
                if Extension.Name = Name then
                    Logger.Print (Name & " is supported!");
                    Found := True;
                    exit;
                end if;
            end loop;

            if not Found then
                Logger.Print (Name & " is not supported!");
                return False;
            end if;
        end loop;

        Logger.Print("---- Supported Layers ----");
        Logger.Print(Supported_Layers);

        for Name of Requested_Layers loop
            Found := False;

            for Layer of Supported_Layers loop
                if Layer.Name = Name then
                    Logger.Print (Name & " is supported!");
                    Found := True;
                    exit;
                end if;
            end loop;

            if not Found then
                Logger.Print (Name & " is not supported!");
                return False;
            end if;
        end loop;

        return True;
    end Supported;

    procedure Initialize (
        Instance : in out Vulkan.Instance;
        SDL_Extensions : SDL_Vulkan.Extension_Name_Arrays) is
        
        App_Info : aliased Vulkan.Application_Info;
        Requested_Extensions, Requested_Layers : Vulkan.String_Vectors.Vector;
        Create_Info : Vulkan.Instance_Create_Info;
        Version : constant Vulkan.Version_Number := Vulkan.Core.Enumerate_Instance_Version;
        use type Vulkan.Result;
        Result : Vulkan.Result;
    begin
        
        App_Info.Application_Name := To_Unbounded_String ("Ye New e Tourney");
        App_info.Application_Version := Version;
        App_Info.Engine_Name      := To_Unbounded_String ("Real Engine");
        App_Info.Engine_Version   := Vulkan.Create_Version (1, 0);
        App_Info.API_Version      := Version;

        for Name of SDL_Extensions loop
            Requested_Extensions.Append (Name.To_String);
        end loop;
        Requested_Extensions.Append ("VK_EXT_debug_utils");

        Create_Info.Application_Info := App_Info'Unchecked_Access;

        Requested_Layers.Append ("VK_LAYER_KHRONOS_validation");

        if not Supported (Requested_Extensions, Requested_Layers) then
            Logger.Print("Something went wrong.");
        end if;

        Create_Info.Enabled_Extension_Names := Requested_Extensions;

        Create_Info.Enabled_Layer_Names := Requested_Layers;

        Result := Vulkan.Core.Create (Create_Info, Instance);

        if Result = Vulkan.Success then
            Logger.Print ("Created an instance!");
        else
            Logger.Print ("Failed to create an instance!");
        end if; 

    end Initialize;

    procedure Destroy (Instance : in out Vulkan.Instance) is
    begin
        Vulkan.Core.Destroy (Instance);
    end Destroy;

end Instances;
