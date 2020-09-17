<template>
  <div class="container">
    <h1 class="title">Activity Search</h1>
      <b-field group-multiline grouped>
        <b-field label="Enter a location" expanded>
          <input class="input" type="text" placeholder="Enter a location" id="autocompleteLocation"/>
        </b-field>
        <b-field label="Max distance (km)">
          <b-numberinput v-model="maxDistance" type="is-primary" :min="1" :max="200"></b-numberinput>
        </b-field>
        <br>
      </b-field>
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
    import api from "../../Api";
    import router from "../../router";

    export default {
        name: "ActivitySearch",
        components: {
            MapPane
        },
        data() {
            return {
              maxDistance: 50
            }
        },
        methods: {
            clearLocation() {
                console.log("needs to be implemented")
            },
            search() {
                console.log("needs to be implemented")
            },
            setDefaultProfileLocation() {
                api.getProfile(this.id, localStorage.getItem('authToken'))
                    .then((response) => {
                        let location = response.data.location;
                        this.profileLocationLatLong = {lat: location.lat(), lng: location.lng()};
                    })
                    .catch(() => {
                        this.warningToast("Error occurred while getting Profile Location details.");
                        router.go(-1)
                    })
            },
        },
        async mounted() {
            this.google = await googleMapsInit();
            this.setDefaultProfileLocation();
        }
    }

</script>

<style scoped>

</style>