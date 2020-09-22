<template>
    <VueResizable
            :width=width
            :height=height
    >
        <b-button id="resizeButton" @click="resizePane">{{isMinimized ? "Restore Map" : "Minimize Map"}}</b-button>
        <div id="map"></div>
    </VueResizable>
</template>

<script>
    import VueResizable from 'vue-resizable'
    import googleMapsInit from '../../utils/googlemaps'
    //Fake data until API endpoint is set up

    const DEFAULT_LOCATION = {lat: -43.4341, lng: 172.6397}
    const DEFAULT_ZOOM = 4;

    export default {
        name: "MapPane",
        components: {VueResizable},
        props: {
            locationChoiceCoordinates: {
                type: Object,
            },
            address: {
                type: String
            },
            //If pins are being made one at a time this should be a string
            //Else if pins are made all at once this should be a list of strings
            infoWindowContent: {
                type: String
            },
            default_width: {
                type: Number
            },
            default_height: {
                type: Number
            },
            markerEnabled: {
                type: Boolean,
                default: true
            }
        },

        data() {
            return {
                height: this.default_height,
                width: this.default_width,
                locationChoiceMarker: null,
                map: null,
                google: null,
                // keeps track of pins on map
                markers: []
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
                this.height = this.height === 0 ? this.default_height : 0
                this.width = this.width === 0 ? this.default_width : 0
            },
            //Allows user to choose their location by clicking on the map
            setLocationWithMarker(position) {
                if (!this.locationChoiceMarker) {
                    this.locationChoiceMarker = new this.google.maps.Marker({
                        position: position,
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
                if (this.markerEnabled == true) {
                    this.google.maps.event.addListener(this.map, 'click', e => {
                        this.setLocationWithMarker(e.latLng);
                    })
                }
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
                marker.setMap(this.map);
                // add markers to list so that we can select what pins to remove
                this.markers.push(marker);
            },
            //Method that should show users profile, or route to their profile in the future
            openDetailedMarkerView(id) {
                alert(`Opening profile ${id}`)
            },
            setZoomLevel(newAddress) {
                if (newAddress) {
                    let address_parts = newAddress.split(',');
                    let zoomLevel = address_parts.length * 4;
                    this.map.setZoom(zoomLevel)
                } else if (this.address) {
                    let address_parts = this.address.split(',');
                    let zoomLevel = address_parts.length * 4;
                    this.map.setZoom(zoomLevel)
                }
            },
            setZoomWithMarkers() {
                let bounds = new this.google.maps.LatLngBounds();
                for (let i = 1; i < this.markers.length; i++) {
                  bounds.extend(this.markers[i].position);
                }
                this.map.fitBounds(bounds);
            },
            removeMarker() {
                this.locationChoiceMarker.setMap(null)
                this.map.setZoom(DEFAULT_ZOOM);
                this.map.setCenter(DEFAULT_LOCATION)
            },
            // method is used to remove the additional pins from the map when the search is reset
            clearAdditionalMarkers() {
                for (let i = 1; i < this.markers.length; i++) {
                    this.markers[i].setMap(null);
                }
                this.markers = [this.markers[0]];
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