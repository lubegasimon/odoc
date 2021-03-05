Two modules that reference each other:

  $ cat a.mli
  type t
  (** {!B} *)
  $ cat b.mli
  type t
  (** {!A} *)

  $ compile a.mli b.mli

Check that references are resolved:

  $ odoc_print a.odocl | jq '.content.Module.items[0].Type[1].doc[0]'
  {
    "`Paragraph": [
      "None",
      [
        {
          "`Reference": [
            {
              "`Resolved": {
                "`Identifier": {
                  "`Root": [
                    {
                      "`RootPage": "test"
                    },
                    "B"
                  ]
                }
              }
            },
            []
          ]
        }
      ]
    ]
  }
  $ odoc_print b.odocl | jq '.content.Module.items[0].Type[1].doc[0]'
  {
    "`Paragraph": [
      "None",
      [
        {
          "`Reference": [
            {
              "`Resolved": {
                "`Identifier": {
                  "`Root": [
                    {
                      "`RootPage": "test"
                    },
                    "A"
                  ]
                }
              }
            },
            []
          ]
        }
      ]
    ]
  }
