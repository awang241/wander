<template>

    <div>
        <form>
            <input class="input" type="text" placeholder="Enter a location" id="autocompleteLocation"/>
        </form>
    </div>

</template>

<script>

    import googleMapsInit from "../../utils/googlemaps";

    let autocompleteLocation;

    export default {
        name: "AutoCompleteLocation",
        props: ["profileLocation"],
        data() {
            return {
                location : {
                    address: "",
                    latitude: "",
                    longitude: ""
                },
                geocoder: "",
            }
        },
        methods: {

            /** This method sets up the autocomplete. It takes the location from the input field and reformats it to a single string.
             This string is saved to the DOM and will be sent to the backend
             There is a lot of logic within the add listener because Google Maps is not in the same scope as Vue. **/
            initAutoCompleteLocation() {
                let options = {
                    types: ['geocode', 'establishment'],
                };

                autocompleteLocation = new this.google.maps.places.Autocomplete(document.getElementById("autocompleteLocation"), options)
                autocompleteLocation.setFields(['address_components']);
                autocompleteLocation.addListener('place_changed', () => {
                    this.location.address = document.getElementById("autocompleteLocation").value
                    this.checkValidGeoCode(this.location.address)
                })
            },

            checkValidGeoCode(locationAddress) {
                return new Promise((resolve, reject) => {
                    this.geocoder.geocode({'address': locationAddress}, (results, status) => {
                        if (status === 'OK') {
                            this.location.latitude = results[0].geometry.location.lat()
                            this.location.longitude = results[0].geometry.location.lng()
                            this.$emit('locationStringChanged', this.location)
                            resolve(true)
                        } else {
                            reject(false);
                        }
                    })
                })
            },

           async updateLocation(locationAddress) {
                this.location.address = locationAddress
                await this.checkValidGeoCode(locationAddress)
            },

            async setLocation() {
                if (this.profileLocation.address != "") {
                    this.location.address = this.profileLocation.address;
                    document.getElementById("autocompleteLocation").value = this.location.address;
                    await this.checkValidGeoCode(this.location.address)
                    this.$parent.updateMapLocationFromAutoComplete(this.location);
                }
            },

            clearLocation() {
                this.location = {location: "", latitude: "", longitude: ""}
            },

            returnLocation() {
                return this.location;
            }

        },
        async mounted() {
            this.google = await googleMapsInit();
            this.geocoder = new this.google.maps.Geocoder;
            this.initAutoCompleteLocation()
            await this.setLocation()
        }
    }
</script>

<style scoped>

</style>