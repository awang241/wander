<template>
    <div class="container">
        <h1 class="title is-5">Activity Search</h1>

      <div>
          insert reusable component
      </div>
        <MapPane marker-label="Profile Location" :location-choice-coordinates="profileLocationLatLong" v-on:locationChoiceChanged="updateLocation"></MapPane>
        <br>

        <div class="row">
            <br>
            <b-field style="float:left">
                <b-button type="is-danger" @click="clearLocation()">Clear</b-button>
            </b-field>
            <b-field style="float:right">
                <b-button type="is-primary" @click="search()">Search</b-button>
            </b-field>
            <br>
        </div>
        <br/>
    </div>
</template>

<script>
    import googleMapsInit from '../../utils/googlemaps'
    import MapPane from "../MapPane";

    export default {
        name: "ActivitySearch",
        components: {
            MapPane
        },
        data() {
            return {
            }
        },
        methods: {
            clearLocation() {
                console.log("needs to be implemented")
            },
            search() {
                console.log("needs to be implemented")
            }
        },
        async mounted() {
            this.google = await googleMapsInit();
        }

    }

</script>

<style scoped>

</style>