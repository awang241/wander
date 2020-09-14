<template>
  <div class="container">
    <h1 class="title is-5">Edit Your Location </h1>

      <form>
      <input class="input" type="text" placeholder="Enter a location" id="autocompleteLocation"/>
    </form>
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


let autocompleteLocation;

    export default {
        name: "EditLocation",
        props: ["profile"],
        components: {
            MapPane
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
          /** This method sets up the autocomplete. It takes the location from the input field and reformats it to a single string.
           This string is saved to the DOM and will be sent to the backend
           There is a lot of logic within the add listener because Google Maps is not in the same scope as Vue. **/
          initAutoCompleteLocation() {
            let options = {
              types: ['geocode'],
            };
            autocompleteLocation = new this.google.maps.places.Autocomplete(document.getElementById("autocompleteLocation"), options)
            autocompleteLocation.setFields(['address_components']);
            autocompleteLocation.addListener('place_changed', () => {
              var locationArray = autocompleteLocation.getPlace();
              this.location.address = this.formatLocationTextField(locationArray);
              document.getElementById("autocompleteLocation").value = this.location.address;
              this.checkValidGeoCode()
            })
          },

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

          checkValidGeoCode() {
            return new Promise((resolve, reject) => {
              this.geocoder.geocode({'address': this.location.address}, (results, status) => {
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

          async checkValidLocation() {
            let result;
            await this.checkValidGeoCode().then(() => {result = true}).catch(() => {result = false})
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

            // NEED TO SUCCESSFULLY SAVE THE LOCATION IN ANOTHER TASK BEFORE COMPARING IT FOR CHANGES
            // const original = JSON.stringify(this.profile.location);
            this.location.address = document.getElementById("autocompleteLocation").value;
            let check = await this.checkValidLocation();
            console.log(this.location)
            if (this.location.address === "" || this.location.latitude === "" || this.location.longitude === "") {
              this.warningToast("Please enter a location")
            // } else if (JSON.stringify((this.location)) === original) {
            //
            //   this.warningToast("No changes made")
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
            }
          }
        },
      async mounted() {
        this.google = await googleMapsInit();
        this.geocoder = new this.google.maps.Geocoder;
        this.setLocation();
        this.initAutoCompleteLocation();
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