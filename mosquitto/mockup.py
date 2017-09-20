import paho.mqtt.client as mqtt
import json, struct, random

# Load configuarion
with open("../web/config.json") as cfile:
    config = json.load(cfile)

# Called when client is connecteed
def on_connect(client, userdata, flags, rc):
    # Go through all rooms
    for room in config["rooms"]:
        # Go through all items
        for item in room["items"]:
            # If item is a light, set on/off randomly
            if item["mqtt_id"].startswith("light/"):
                d = struct.pack('b',random.randint(0,1))
                client.publish(item["mqtt_id"], d, retain=True, qos=1)
            # It item is a music player, set example-values
            elif item["mqtt_id"].startswith("mpd/"):
                client.publish(item["mqtt_id"], '{"current_song":"Test Song", "playlist_name":"Test Playlist"}', retain=True, qos=1)
    # Disconnect from client, ending script
    client.disconnect()

# Create client
client = mqtt.Client(transport="websockets")
client.on_connect = on_connect
# connect to broker
client.connect("127.0.0.1", 9000, 5)
# wait until client disconnects
client.loop_forever()
