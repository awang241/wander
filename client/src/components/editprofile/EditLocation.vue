<template>
  <div class="container">
    <h1 class="title is-5">Edit Your Location </h1>

    <AutoCompleteLocation v-on:updateMap="updateLocation" v-bind:profileLocation="this.profile.location" ref="autocomplete"></AutoCompleteLocation>

    <MapPane v-bind:address="this.locationString" marker-label="Profile Location" :location-choice-coordinates="profileLocationLatLong" v-on:locationChoiceChanged="updateLocation"></MapPane>
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
              geocoder: null,
              locationString: this.profile.location.address
            }
        },
        methods: {

          updateLocation(location) {
            this.profileLocationLatLong = {lat: location.lat(), lng: location.lng()}
            this.geocoder.geocode({'location': this.profileLocationLatLong}, function(results, status) {
              if (status === 'OK') {
                document.getElementById("autocompleteLocation").value = results[0].formatted_address
                this.locationString = results[0].formatted_address
              }
            })
          },

          async checkValidLocation(locationAddress) {
            let result;
            await this.$refs.autocomplete.checkValidGeoCode(locationAddress).then(() => {result = true}).catch(() => {result = false})
            return result
          },

          updateMapLocationFromAutoComplete(location) {
            this.profileLocationLatLong = {lat: location.latitude, lng: location.longitude}
            this.locationString = location.address
          },

          clearLocation() {
            this.$parent.clearLocation();
            document.getElementById("autocompleteLocation").value = null;
            this.profileLocationLatLong = ""
            this.locationString = ""
            this.$refs.autocomplete.clearLocation();
          },

          async submitLocation() {
            //Using JSON methods to make a constant and compare two JSON objects
            const original = JSON.stringify(this.profile.location.address);
            this.$refs.autocomplete.updateLocation(document.getElementById("autocompleteLocation").value);
            let check = await this.checkValidLocation(document.getElementById("autocompleteLocation").value);
            const location = this.$refs.autocomplete.returnLocation();

            if (location.address === "" || location.latitude === "" || location.longitude === "") {
              this.warningToast("Please enter a location")
            } else if (JSON.stringify((location.address)) === original) {
              this.warningToast("No changes made")
            } else if(check == false) {
              this.warningToast("Location is invalid, please use the auto-complete suggestions")
            } else {
              this.$parent.updateLocation(location)
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