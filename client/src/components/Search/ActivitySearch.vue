<template>
    <div class="container">
        <h1 class="title is-5">Activity Search</h1>

        <form>
            <input class="input" type="text" placeholder="Search by address" id="autocompleteLocation"/>
        </form>
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
    import toastMixin from "../../mixins/toastMixin";
    import googleMapsInit from '../../utils/googlemaps'
    import MapPane from "../MapPane";

    let autocompleteLocation;

    export default {
        name: "ActivitySearch",
        components: {
            MapPane
        },
        mixins: [toastMixin],
        data() {
            return {
                location: "",
                google: null,
                profileLocationLatLong: null,
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
                    this.location = locationString
                    document.getElementById("autocompleteLocation").value = locationString;
                    let geocoder = new this.google.maps.Geocoder;
                    geocoder.geocode({'address': document.getElementById("autocompleteLocation").value}, (results, status) => {
                        if (status === 'OK') {
                            this.profileLocationLatLong = {lat: results[0].geometry.location.lat(), lng: results[0].geometry.location.lng()}
                        }
                    })
                })
            },

            updateLocation(location) {
                this.profileLocationLatLong = {lat: location.lat(), lng: location.lng()}
                let geocoder = new this.google.maps.Geocoder;
                let latlng = {lat: parseFloat(location.lat()), lng: parseFloat(location.lng())};
                geocoder.geocode({'location': latlng}, function(results, status) {
                    if (status === 'OK') {
                        document.getElementById("autocompleteLocation").value = results[0].formatted_address
                    }
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

</style>