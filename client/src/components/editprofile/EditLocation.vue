<template>
  <div class="container">
    <h1 class="title is-5">Edit Your Location </h1>

        <MapPane/>

        <br>
        <br>
        <br>

        <form>

            <div class="select" id="countrySelectMenu">
                <select v-model="DropdownCountry" id="country" v-on:change="setAutoCompleteCountry" required expanded>
                    <option value="" disabled selected> Must select a country</option>
                    <option v-for="country in possibleCountries" :key="country">
                        {{country}}
                    </option>
                </select>
            </div>
            <br>
            <br>

            <input class="input" type="text" placeholder="Enter a city (required)" id="autocompleteCities"/>
            <br>
            <br>
            <input onkeypress="return /[a-z ]/i.test(event.key)" class="input" type="text" placeholder="Enter a state (optional)" id="autocompleteStates"/>

        </form>
    <form>
      <input class="input" type="text" placeholder="Enter a location" id="autocompleteLocation"/>
    </form>

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
                location: "",
                google: null
            }
        },
        methods: {
            /** This method sets up the autocomplete. It takes the location from the input field and reformats it to a single string.
             This string is saved to the DOM and will be sent to the backend
             There is a lot of logic within the add listener because Google Maps is not in the same scope as Vue. **/
            initAutoCompleteLocation() {
                let options = {
                    types: ['geocode'],
                }
                // eslint-disable-next-line no-undef
                autocompleteLocation = new this.google.maps.places.Autocomplete(document.getElementById("autocompleteLocation"), options)
                autocompleteLocation.setFields(['address_components'])
                autocompleteLocation.addListener('place_changed', function () {
                    var locationArray = autocompleteLocation.getPlace();


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
        document.getElementById("autocompleteLocation").value = locationString;
      })
    },

    clearLocation() {
      this.$parent.clearLocation()
      this.successToast("Location removed")
      document.getElementById("autocompleteLocation").value = null;
      this.location = {location: ""}
    },
    submitLocation() {
      //Using JSON methods to make a constant and compare two JSON objects
      const original = JSON.stringify(this.profile.location)
      this.location = document.getElementById("autocompleteLocation").value;
      if (this.location === "") {
        this.warningToast("Please enter a location")
      } else if (JSON.stringify((this.location)) === original) {
        this.warningToast("No changes made")
      } else {
        this.$parent.updateLocation(this.location)
        this.successToast("New location saved")
      }
    },
    setLocation() {
      if (this.profile.location != null) {
        this.location = this.profile.location;
        document.getElementById("autocompleteLocation").value = this.location;
      }
    }
  },
  async mounted() {
    this.google = await googleMapsInit();
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