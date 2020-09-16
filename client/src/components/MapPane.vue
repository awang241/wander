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
                this.setLocationWithMarker(newCoords)
                this.map.setCenter(newCoords)
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
                if (this.locationChoiceMarker) {
                    this.locationChoiceMarker.setPosition(position)
                } else {
                    this.locationChoiceMarker = new this.google.maps.Marker({
                        position: position,
                        label: {text: this.markerLabel},
                    });
                    this.locationChoiceMarker.setMap(this.map)
                }
              this.setZoomLevel()
              this.$emit('locationChoiceChanged', position)
            },
            //Dynamically creates the google map
            createMap() {
                const defaultLocation = {lat: -43.4341, lng: 172.6397}
                this.map = new this.google.maps.Map(document.getElementById('map'), {
                    zoom: 4,
                    center: this.locationChoiceCoordinates ? this.locationChoiceCoordinates : defaultLocation,
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
                const marker = new this.google.maps.Marker({
                    position: position,
                    map: this.map,
                    label: {text: text},
                    id: id
                });
                marker.setMap(this.map)
                marker.addListener('click', () => this.openDetailedMarkerView(id))
            },
            //Method that should show users profile, or route to their profile in the future
            openDetailedMarkerView(id) {
                alert(`Opening profile ${id}`)
            },
            setZoomLevel() {
              let address_parts = this.address.split(',')
              let zoomLevel = address_parts.length * 3
              if (this.map.getZoom() < zoomLevel) {
                this.map.setZoom(zoomLevel)
              }
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