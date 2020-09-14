<template>
  <div class="container">
    <h1 class="title is-5">Edit Your Location </h1>

    <AutoCompleteLocation></AutoCompleteLocation>

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
              location: {
                address: "",
                latitude: "",
                longitude: ""
              },
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

          formatLocationTextField(locationArray) {
            let locationString = "";
            for (let i = 0; i < (locationArray.address_components).length; i++) {
              if (i === 0) {
                locationString = locationArray.address_components[0].long_name;
              } else if (i !== (locationArray.address_components).length) {
                if (locationArray.address_components[i].long_name !== locationArray.address_components[i - 1].long_name) {
                  locationString = locationString + ", " + locationArray.address_components[i].long_name;
                }
              }
            }
            return locationString;
          },

          checkValidGeoCode(locationAddress) {
            return new Promise((resolve, reject) => {
              this.geocoder.geocode({'address': locationAddress}, (results, status) => {
                if (status === 'OK') {
                  this.location.latitude = results[0].geometry.location.lat()
                  this.location.longitude = results[0].geometry.location.lng()
                  this.profileLocationLatLong = {lat: this.location.latitude, lng: this.location.longitude}
                  resolve(true)
                } else {
                  reject(false);
                }
              })
            })
          },

          async checkValidLocation(locationAddress) {
            let result;
            await this.checkValidGeoCode(locationAddress).then(() => {result = true}).catch(() => {result = false})
            return result
          },

          clearLocation() {
            this.$parent.clearLocation();
            this.successToast("Location removed");
            document.getElementById("autocompleteLocation").value = null;
            this.location = {location: "", latitude: "", longitude: ""}
          },

          async submitLocation() {
            //Using JSON methods to make a constant and compare two JSON objects
            const original = JSON.stringify(this.profile.location.address);
            this.location.address = document.getElementById("autocompleteLocation").value;
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
          },

          setLocation() {
            if (this.profile.location.address != "") {
              this.location.address = this.profile.location.address;
              document.getElementById("autocompleteLocation").value = this.location.address;
              this.checkValidGeoCode(this.location.address)
            }
          }
        },
      async mounted() {
        this.google = await googleMapsInit();
        this.geocoder = new this.google.maps.Geocoder;
        this.setLocation();
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