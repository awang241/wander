<template>
    <VueResizable
            :width=width
            :height=height
    >
        <b-button id="resizeButton" @click="resizePane">{{isMinimized ? "Restore Map" : "Minimize Map"}}</b-button>
        <div id="map"></div>
        <div id="legend" v-if="$parent.$options.name == 'ActivitySearch'"></div>

    </VueResizable>
</template>

<script>
    import VueResizable from 'vue-resizable'
    import googleMapsInit from '../../utils/googlemaps'
    import {viewActivity} from "../../main";

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
                markers: [],
                icons: {
                  searchBaseIcon: {
                    name: 'Search Centre',
                    icon: 'http://labs.google.com/ridefinder/images/mm_20_red.png'
                  },
                  activityIcon: {
                    name: 'Activity',
                    icon: 'http://labs.google.com/ridefinder/images/mm_20_blue.png'
                  },
                }
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
                        icon: this.icons.searchBaseIcon.icon
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
                if (this.markerEnabled) {
                    this.google.maps.event.addListener(this.map, 'click', e => {
                        this.setLocationWithMarker(e.latLng);
                    })
                }

              this.createLegend();
            },
            //Creates a singular marker on the map
            createSingleMarker({position, id}, infoWindowContent) {
                const infowindow = new this.google.maps.InfoWindow({
                    content: infoWindowContent
                });
                const marker = new this.google.maps.Marker({
                    position: position,
                    map: this.map,
                    id: id,
                    icon: this.icons.activityIcon.icon
                });

                marker.addListener("mouseover", () => {
                    infowindow.open(this.map, marker);
                });
                marker.addListener("mouseout", () => {
                    infowindow.close();
                });
                marker.addListener('click', () => this.openActivityModal(id))
                marker.setMap(this.map);
                // add markers to list so that we can select what pins to remove
                this.markers.push(marker);
            },
            createLegend(){
              let legend = document.getElementById('legend');
              for (let key in this.icons) {
                let type = this.icons[key];
                let name = type.name;
                let icon = type.icon;
                let div = document.createElement('div');
                div.innerHTML = '<img src="' + icon + '"> ' + name;
                legend.appendChild(div);
              }
              this.map.controls[this.google.maps.ControlPosition.LEFT_BOTTOM].push(legend);
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
            },
            openActivityModal(id) {
                this.$buefy.modal.open({
                    parent: this,
                    props: {idProp: id, viewingThroughModal: true},
                    component: viewActivity,
                    trapFocus: true,
                    scroll: "clip"
                })
            },

        }
    }
</script>

<style scoped>
    #map {
        width: 100%;
        height: 90%;
        position: relative;
    }

    #legend {
      font-family: Arial, sans-serif;
      background: #fff;
      padding: 10px;
      margin: 10px;
      border: 1px solid #000;
    }

</style>