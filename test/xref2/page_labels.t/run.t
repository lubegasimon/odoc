  $ odoc compile page.mld
  $ odoc link page-page.odoc
  $ odoc_print page-page.odocl | jq '.content[1]["`Paragraph"][1][0]["`Reference"][0]'
  {
    "`Resolved": {
      "`Identifier": {
        "`Label": [
          {
            "`RootPage": "page"
          },
          "test"
        ]
      }
    }
  }


