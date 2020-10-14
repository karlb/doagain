
var jsonSchema = {
  "$id": "http://doagain.karl.berlin/schema.json",
  "type": "object",
  "definitions": {},
  "$schema": "http://json-schema.org/draft-07/schema#",
  "properties": {
  "entries": {
    "$id": "/properties/entries",
    "type": "array",
    "items": {
    "$id": "/properties/entries/items",
    "type": "object",
    "properties": {
      "description": {
      "$id": "/properties/entries/items/properties/description",
      "type": "string",
      "default": "",
      "examples": [
        "bar"
      ]
      },
      "completed": {
      "$id": "/properties/entries/items/properties/completed",
      "type": "boolean",
      "default": false,
      "examples": [
        false
      ]
      },
      "editing": {
      "$id": "/properties/entries/items/properties/editing",
      "type": "boolean",
      "default": false,
      "examples": [
        false
      ]
      },
      "id": {
      "$id": "/properties/entries/items/properties/id",
      "type": "integer",
      "default": 0,
      "examples": [
        11
      ]
      },
      "doneAt": {
      "$id": "/properties/entries/items/properties/doneAt",
      "type": "array",
      "items": {
        "$id": "/properties/entries/items/properties/doneAt/items",
        "type": "integer",
        "title": "List of unixtimes when the item has been done",
        "default": 0,
        "examples": [
          1493360866108,
          1493360862018
        ]
      }
      },
      "tags": {
      "$id": "/properties/entries/items/properties/tags",
      "type": "array"
      }
    }
    }
  },
  "field": {
    "$id": "/properties/field",
    "type": "string",
    "title": "Input in the text field",
    "default": "",
    "examples": [
      ""
    ]
  },
  "uid": {
    "$id": "/properties/uid",
    "type": "integer",
    "title": "ID for next added element",
    "default": 0,
    "examples": [
      46
    ]
  },
  "visibility": {
    "$id": "/properties/visibility",
    "type": ["string", "null"],
    "title": "Which category is shown",
    "default": "",
    "examples": [
      "food"
    ]
  }
  }
}


var RemotestorageDoagain = { name: 'doagain', builder: function(privateClient, publicClient) {

  privateClient.declareType('doagain-store', jsonSchema);

  return {
    exports: {
      save: function(state) {
        return privateClient.storeObject('doagain-store', 'doagain.json', state);
      },
      load: function() {
        return privateClient.getObject('doagain.json');
      },
      onStateChange: function(callback) {
        privateClient.on('change', function (evt) {
          if (evt.origin === 'remote' || evt.origin === 'conflict') {
            console.debug('Use remote data:', evt.origin, evt.newValue);
            callback(evt.newValue);
            if (evt.origin === 'conflict') {
              // Create a backup for the case that important data has
              // been lost during conflict resolution.
              localStorage.setItem(
                  'doagain-backup-' + (new Date()).toISOString(),
                  JSON.stringify(evt.oldValue)
              );
              // Keep all new entries and add old entries which are not
              // present in the new data.
              var newDescriptions = evt.newValue.entries.map(function (e) {return e.description});
              evt.oldValue.entries.forEach(function(entry) {
                if (newDescriptions.indexOf(entry.description) === -1) {
                  evt.newValue.entries.push(entry);
                }
              });
              evt.newValue.uid = Math.max(evt.oldValue.uid, evt.newValue.uid)
            }
          }
        });
      },
    }
  }
}};
