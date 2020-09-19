<template>
    <VueResizable
            style="border: 2px solid black;"
            :width=width
            :height=height
    >
        <b-button id="resizeButton" @click="resizePane">{{isMinimized ? "Restore Map" : "Minimize Map"}}</b-button>
        <div id="map"></div>
    </VueResizable>
</template>

<script>
    import VueResizable from 'vue-resizable'
    import googleMapsInit from '../utils/googlemaps'
    //Fake data until API endpoint is set up

    const DEFAULT_HEIGHT = 500;
    const DEFAULT_WIDTH = 1075;
    const DEFAULT_LOCATION = {lat: -43.4341, lng: 172.6397}
    const DEFAULT_ZOOM = 4;

    const locations = [
        {
            position: {
                lat: 48.160910,
                lng: 16.383330,
            },
            text: "Marker one",
            id: 7
        },
        {
            position: {
                lat: 68.174270,
                lng: 16.329620,
            },
            text: "Marker two",
            id: 8
        },
    ];
    export default {
        name: "MapPane",
        components: {VueResizable},
        props: {
            locationChoiceCoordinates: {
                type: Object,
            },
            markerLabel: {
                type: String,
                default: "Location"
            },
            address: {
                type: String
            },
            //If pins are being made one at a time this should be a string
            //Else if pins are made all at once this should be a list of strings
            infoWindowContent: {
                type: String
            }
        },

        data() {
            return {
                height: DEFAULT_HEIGHT,
                width: DEFAULT_WIDTH,
                locationChoiceMarker: null,
                map: null,
                google: null
            }
        },

        watch: {
            locationChoiceCoordinates: function (newCoords) {
                if (newCoords) {
                  this.setLocationWithMarker(newCoords)
                  this.map.setCenter(newCoords)
                }
            }
        },

        async mounted() {
            this.google = await googleMapsInit()
            await this.createMap()
            this.createMarkers()
            if (this.locationChoiceCoordinates) {
                this.setLocationWithMarker(this.locationChoiceCoordinates)
            }
        },
        computed: {
            isMinimized() {
                return this.height === 0 && this.width === 0
            },
        },
        methods: {
            //Minimizes the map pane if it is not already minimized
            //Restores the pane to its default size if it is minimized
            resizePane() {
                this.height = this.height === 0 ? DEFAULT_HEIGHT : 0
                this.width = this.width === 0 ? DEFAULT_WIDTH : 0
            },
            //Allows user to choose their location by clicking on the map
            setLocationWithMarker(position) {
              if (!this.locationChoiceMarker) {
                this.locationChoiceMarker = new this.google.maps.Marker({
                  position: position,
                  label: {text: this.markerLabel},
                });
                this.locationChoiceMarker.setMap(this.map)
              } else if (this.locationChoiceMarker.map === null) {
                this.locationChoiceMarker.setMap(this.map)
              } else {
                this.locationChoiceMarker.setPosition(position)
              }
              this.setZoomLevel()
              this.$emit('locationChoiceChanged', position)
            },
            //Dynamically creates the google map
            createMap() {
                this.map = new this.google.maps.Map(document.getElementById('map'), {
                    zoom: DEFAULT_ZOOM,
                    center: this.locationChoiceCoordinates ? this.locationChoiceCoordinates : DEFAULT_LOCATION,
                });
                this.google.maps.event.addListener(this.map, 'click', e => {
                    this.setLocationWithMarker(e.latLng);
                })
            },
            //Loops through locations and creates marker for each one
            createMarkers() {
                locations.forEach(location => this.createSingleMarker(location))
            },
            //Creates a singular marker on the map
            createSingleMarker({position, text, id}) {
                //content is just a place holder
                const infowindow = new this.google.maps.InfoWindow({
                    content: "contentString"
                });
                const marker = new this.google.maps.Marker({
                    position: position,
                    map: this.map,
                    label: {text: text},
                    id: id
                });
                marker.addListener("click", () => {
                    infowindow.open(this.map, marker);
                });
                marker.setMap(this.map)
            },
            //Method that should show users profile, or route to their profile in the future
            openDetailedMarkerView(id) {
                alert(`Opening profile ${id}`)
            },
            setZoomLevel(newAddress) {
                if (newAddress){
                    let address_parts = newAddress.split(',');
                    let zoomLevel = address_parts.length * 3;
                    this.map.setZoom(zoomLevel)
                }

                else if (this.address) {
                    let address_parts = this.address.split(',');
                    let zoomLevel = address_parts.length * 3;
                    this.map.setZoom(zoomLevel)
                }
            },
            removeMarker() {
                this.locationChoiceMarker.setMap(null)
                this.map.setZoom(DEFAULT_ZOOM);
                this.map.setCenter(DEFAULT_LOCATION)
            }
        }
    }
</script>

<style scoped>
    #map {
        width: 100%;
        height: 90%;
        position: relative;
    }
</style>