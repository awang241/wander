<template>
    <VueResizable
            style="border: 2px solid black;"
            fit-parent="true"
            :width=width
            :height=height
    >
        <b-button @click="resizePane">{{isMinimized ? "Restore Map" : "Minimize Map"}}</b-button>
        <iframe
                id="map"
                src="https://www.google.com/maps/embed/v1/place?key=AIzaSyAWLmvfFRHMuF-D9nmVTAQUYASaUtQoPqQ
            &q=Space+Needle,Seattle+WA" allowfullscreen>
        </iframe>
    </VueResizable>
</template>

<script>
    import VueResizable from 'vue-resizable'

    export default {
        name: "MapPane",
        components: {VueResizable},
        data() {
            return {
                height: 500,
                width: 500
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
                this.height = this.height === 0 ? 500 : 0
                this.width = this.width === 0 ? 500 : 0
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