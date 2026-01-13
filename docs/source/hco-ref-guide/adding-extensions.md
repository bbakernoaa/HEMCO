# Adding New HEMCO Extensions

This guide provides end-to-end instructions for adding new extensions to the HEMCO model.

## 1. Create a New Extension File

Start by copying the extension template file, `src/Extensions/hcox_template_mod.F90x`, to a new file in the same directory. The new file should be named `hcox_<yourname>_mod.F90`, where `<yourname>` is the name of your extension.

## 2. Update the Extension Module

Modify the new `hcox_<yourname>_mod.F90` file to implement your extension's functionality. This includes:

- **Module Name:** Change `HCOX_template_mod` to `HCOX_<yourname>_mod`.
- **Subroutine Names:** Replace all instances of `HCOX_template_` with `HCOX_<yourname>_`.
- **Instance Type:** Rename the `TemplateInst` derived type to something more descriptive of your extension.
- **Implement Logic:** Add your extension's logic to the `Run`, `Init`, and `Final` subroutines.

## 3. Update the Extension State

In `src/Extensions/hcox_state_mod.F90`, add a new integer member to the `Ext_State` derived type. This member will hold the instance handle for your extension. For example:

```fortran
TYPE, PUBLIC :: Ext_State
    ...
    INTEGER :: <yourname> ! Your new extension
    ...
END TYPE Ext_State
```

Also, initialize the new member to `-1` in the `ExtStateInit` subroutine in the same file.

## 4. Integrate into the Build System

Add your new extension file to the `src/Extensions/CMakeLists.txt` file. This will ensure that your extension is compiled and included in the HEMCO library.

## 5. Register the Extension

In `src/Extensions/hcox_driver_mod.F90`, add calls to your new extension's `Init`, `Run`, and `Final` subroutines in the corresponding driver subroutines. This will register your extension with the HEMCO model.

## 6. Best Practices

- **Code Style:** Follow the coding conventions of the surrounding code.
- **Documentation:** Add comments to your code to explain its functionality.
- **Testing:** Add a test for your new extension to ensure it is working correctly.
