# abap-archiving-engine


## General info

This class allows you to archive (any) archiving object by passing a table of the business object keys to the method "write".

**Use at own risk**

## Important info

Because all of this happens dynamically you should prefer archiving objects that are not "too complex" meaning it doesn't have to many cases where the key field/structure does not exist in a targeted table.

For example you should **avoid this**: `vbak-vbeln = nast-objkey`

Otherwise there should be no problems. The engine has the capability to map key fields of completely different types, but if you decide to use this you should make a test run and look at the protocol of the
method "write". All tables that will be archived are listed here. Generate a test file and look at it if everything seems plausible.

Currently there is no index when archiving business objects; that means there is no way to "read" the archive. But you can reload your data if you need to.

## How to install

1. Use [abapgit](https://docs.abapgit.org/) to clone this repository on to your SAP system.
2. Also clone the [dynamic-select repository](https://github.com/00500500/abap-dynamic-select) and the [string-util repository](https://github.com/00500500/abap-string-util).
3. Build your archiving object (transaction: aobj)
4. Create the write/delete/reload programs

(I didn't explain it in detail intentionally, because you need to know what your doing)

## How to use it

Start your archiving process normally with the transaction SARA or start the programs manually as a background job.

## Pros

If you want another table to be archived you just need to add it's name into the structure definition in your archiving object. That's all! No extra coding or new table needed!

This engine is different from the SAP standard Archiving-Engine, because that would require the user to create a new "Archive-Table" of each table they want to archive.

## Cons

There is only a implementation for some archiving-classes.

Those are:
* CHANGEDOCU
* STATUS
* TEXT
* CLASSIFY

also there is no 100% guarantee that the field mapping of the table keys is working as intended.

## Sample code

### Write program

```
DATA(go_ar_engine) = new zcl_are_engine( VALUE #( archive_object = !NAME OF YOUR ARCHIVE OBJECT!
                                                      test_run = p_test ) ) .
go_ar_engine->write( !TABLE WITH KEYS! ).
```

For example if you want to use the archive object SD_VBAK then you need to pass a table containing the vbeln of the documents you wanna archive.

### Delete program

```
DATA(go_ar_engine) = new zcl_are_engine( VALUE #( archive_object = !NAME OF YOUR ARCHIVE OBJECT!
                                                      test_run = p_test ) ).
go_ar_engine->delete( ).
```

### Reload program

```
DATA(go_ar_engine) = new zcl_are_engine( VALUE #( archive_object = !NAME OF YOUR ARCHIVE OBJECT!
                                                      test_run = p_test ) ).
go_ar_engine->reload( ).
```
