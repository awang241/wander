<template>
  <div class="container">
    <h1 class="title is-5">Edit Your Location </h1>

    <AutoCompleteLocation v-on:updateMap="updateLocation" v-bind:profileLocation="this.profile.location" ref="autocomplete"></AutoCompleteLocation>

    <MapPane marker-label="Profile Location" :location-choice-coordinates="profileLocationLatLong" v-on:locationChoiceChanged="updateLocation"></MapPane>
    <br>

    <div class="row">
      <br>
      <b-field style="float:left">
        <b-button type="is-danger" @click="clearLocation()">Clear fields</b-button>
      </b-field>
      <b-field style="float:right">
        <b-button type="is-primary" @click="submitLocation()">Save</b-button>
      </b-field>
      <br>
    </div>
    <br/>
  </div>
</template>


<script>

import toastMixin from "../../mixins/toastMixin";
import googleMapsInit from '../../utils/googlemaps'
import MapPane from "../MapPane";
import AutoCompleteLocation  from "../AutoCompleteLocation";

    export default {
        name: "EditLocation",
        props: ["profile"],
        components: {
          MapPane,
          AutoCompleteLocation
        },
        mixins: [toastMixin],
        data() {
            return {
              google: null,
              profileLocationLatLong: null,
              geocoder: null
            }
        },
        methods: {

          updateLocation(location) {
            this.profileLocationLatLong = {lat: location.lat(), lng: location.lng()}
            this.geocoder.geocode({'location': this.profileLocationLatLong}, function(results, status) {
              if (status === 'OK') {
                document.getElementById("autocompleteLocation").value = results[0].formatted_address
              }
            })
          },

          async checkValidLocation(locationAddress) {
            let result;
            await this.$refs.autocomplete.checkValidGeoCode(locationAddress).then(() => {result = true}).catch(() => {result = false})
            return result
          },

          updateMapLocationFromAutoComplete(location) {
            console.log("brap brap braaaaapp")
            this.profileLocationLatLong = {lat: location.latitude, lng: location.longitude}
          },

          clearLocation() {
            this.$parent.clearLocation();
            this.successToast("Location removed");
            document.getElementById("autocompleteLocation").value = null;
            this.$refs.autocomplete.clearLocation();
          },

          async submitLocation() {
            //Using JSON methods to make a constant and compare two JSON objects
            const original = JSON.stringify(this.profile.location.address);
            this.$refs.autocomplete.updateLocation(document.getElementById("autocompleteLocation").value);
            let check = await this.checkValidLocation(this.location.address);
            if (this.location.address === "" || this.location.latitude === "" || this.location.longitude === "") {
              this.warningToast("Please enter a location")
            } else if (JSON.stringify((this.location.address)) === original) {
              this.warningToast("No changes made")
            } else if(check == false) {
              this.warningToast("Location is invalid, please use the auto-complete suggestions")
            } else {
              this.$parent.updateLocation(this.location)
            }
          }



        },
      async mounted() {
        this.google = await googleMapsInit();
        this.geocoder = new this.google.maps.Geocoder;
      }

}

</script>

<style scoped>
.container {
  background-color: #F7F8F9;
  margin-top: 0px;
  padding: 0px;
}
</style>