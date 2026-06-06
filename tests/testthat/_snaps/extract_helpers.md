# edstr_extract: extract and note output are stable (refactor oracle)

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["n", "doc_id", "id_group", "texte", "diabete", "cancer", "concept", "extract"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["1", "2", "3", "4", "5"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["1", "2", "3", "5", "6"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 5, 6]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["<p class=\"t\">Patient diabetique, cancer du sein gauche<\/p>", "<p class=\"t\">Pas de diabete retrouve au bilan<\/p>", "<p class=\"t\">Cancer du sein droit, metastases<\/p>", "<p class=\"t\">Diabete de type 2 ancien et cancer<\/p>", "<p class=\"t\">Suivi cancer, diabete insulino-requerant<\/p>"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 0, 1, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 0, 1, 1, 1]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["cancer ; diabete", "diabete", "cancer", "cancer ; diabete", "cancer ; diabete"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["diabetique ; cancer", "diabete", "Cancer", "Diabete ; cancer", "cancer ; diabete"]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["n", "doc_id", "id_group", "texte", "concept", "extract"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["1", "2", "3", "4", "5"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["1", "2", "3", "5", "6"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 5, 6]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["<p class=\"t\">Patient <span class='extract diabete'>diabetique<\/span>, <span class='extract cancer'>cancer<\/span> du sein gauche<\/p>", "<p class=\"t\">Pas de <span class='extract diabete'>diabete<\/span> retrouve au bilan<\/p>", "<p class=\"t\"><span class='extract cancer'>Cancer<\/span> du sein droit, metastases<\/p>", "<p class=\"t\"><span class='extract diabete'>Diabete<\/span> de type 2 ancien et <span class='extract cancer'>cancer<\/span><\/p>", "<p class=\"t\">Suivi <span class='extract cancer'>cancer<\/span>, <span class='extract diabete'>diabete<\/span> insulino-requerant<\/p>"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["cancer ; diabete", "diabete", "cancer", "cancer ; diabete", "cancer ; diabete"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["<span class='extract diabete'>diabetique<\/span>  <span class='extract cancer'>cancer<\/span>", "<span class='extract diabete'>diabete<\/span>", "<span class='extract cancer'>Cancer<\/span>", "<span class='extract diabete'>Diabete<\/span>  <span class='extract cancer'>cancer<\/span>", "<span class='extract cancer'>cancer<\/span>  <span class='extract diabete'>diabete<\/span>"]
        }
      ]
    }

