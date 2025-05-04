-------------------------------------------------------------------------------
--  Instance utility functions
-------------------------------------------------------------------------------
with Vulkan;
with SDL_Backend; use SDL_Backend;

package Instances is

    ---------------------------------------------------------------------------
    --  Make an instance
    ---------------------------------------------------------------------------
    procedure Initialize (
        Instance : in out Vulkan.Instance;
        SDL_Extensions : SDL_Vulkan.Extension_Name_Arrays); 

    ---------------------------------------------------------------------------
    --  Destroy an instance
    ---------------------------------------------------------------------------
    procedure Destroy (Instance : in out Vulkan.Instance);
end Instances;
-------------------------------------------------------------------------------
