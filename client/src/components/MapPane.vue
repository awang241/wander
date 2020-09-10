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
        data() {
            return {
                height: 500,
                width: 500,
                myLocationMarker: null,
                map: null,
                google: null
            }
        },
        async mounted() {
            this.google = await googleMapsInit()
            this.createMap()
            this.createMarkers()
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
                this.height = this.height === 0 ? 500 : 0
                this.width = this.width === 0 ? 500 : 0
            },
            //Allows user to choose their location by clicking on the map
            setLocationWithMarker(position){
                if(this.myLocationMarker){
                    this.myLocationMarker.setPosition(position)
                } else {
                    this.myLocationMarker = new this.google.maps.Marker({
                        position: position,
                        label: {text: "My location"},
                    });
                    this.myLocationMarker.setMap(this.map)
                }
            },
            //Dynamically creates the google map
            createMap(){
                const initialLocation = {lat: -25.363, lng: 13.044}
                this.map = new this.google.maps.Map(document.getElementById('map'), {
                    zoom: 4,
                    center: initialLocation,
                });
                this.google.maps.event.addListener(this.map, 'click', e => {
                    this.setLocationWithMarker(e.latLng);
                })
            },
            //Loops through locations and creates marker for each one
            createMarkers(){
                //Api call would go here
                locations.forEach(location => this.createSingleMarker(location))
            },
            //Creates a singular marker on the map
            createSingleMarker({position, text, id}){
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
            openDetailedMarkerView(id){
                alert(`Opening profile ${id}`)
            }
        }
    }
</script>

<style scoped>
    #map {
        width: 100%;
        height: 100%;
        position: relative;
    }
</style>